(ns hf.depstar.uberjar
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io InputStream PushbackReader)
           (java.nio.file CopyOption LinkOption OpenOption
                          StandardCopyOption StandardOpenOption
                          FileSystem FileSystems Files
                          FileVisitOption FileVisitResult FileVisitor
                          Path)
           (java.nio.file.attribute FileAttribute FileTime)
           (java.util.jar JarInputStream JarEntry))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:dynamic ^:private *debug* nil)
(def ^:dynamic ^:private *suppress-clash* nil)
(def ^:dynamic ^:private *verbose* nil)

(defn env-prop
  "Given a setting name, get its Boolean value from the environment,
  validate it, and return the value (or nil if no setting is present)."
  [setting]
  (let [env-setting  (str "DEPSTAR_" (str/upper-case setting))
        prop-setting (str "depstar." (str/lower-case setting))]
    (when-let [level (or (System/getenv env-setting)
                         (System/getProperty prop-setting))]
      (case level
        "true"  true
        "false" false ;; because (if (Boolean. "false") :is-truthy :argh!)
        (throw (ex-info (str "depstar " setting
                             " should be true or false")
                        {:level    level
                         :env      (System/getenv env-setting)
                         :property (System/getProperty prop-setting)}))))))

(defonce ^FileSystem FS (FileSystems/getDefault))

;; could add (StandardOpenOption/valueOf "SYNC") here as well but that
;; could slow things down (and hasn't yet proved to be necessary)
(def open-opts (into-array OpenOption [(StandardOpenOption/valueOf "CREATE")]))

(def copy-opts (into-array CopyOption [(StandardCopyOption/valueOf "REPLACE_EXISTING")]))

(def visit-opts (doto
                 (java.util.HashSet.)
                 (.add (FileVisitOption/valueOf "FOLLOW_LINKS"))))

(defonce errors (atom 0))

(defonce multi-release? (atom false))

(defn path
  ^Path [s]
  (.getPath FS s (make-array String 0)))

(def ^:private idiotic-log4j2-plugins-file
  "Log4j2 has a very problematic binary plugins cache file that needs to
  be merged -- but it's going away in 3.0.0 apparently because it keeps
  breaking build tools... As a workaround, if we run into multiple copies
  of it, we will let the larger file overwrite the smaller file so that
  we are likely to end up with more plugins in the final artifact. This
  is not a real solution -- we should rebuild the PluginsCache object
  but I don't want to face that right now. What we actually do here is
  just allow 'small' versions of this file to be overwritten."
  "META-INF/org/apache/logging/log4j/core/config/plugins/Log4j2Plugins.dat")

(def ^:private idiotic-log4j2-plugins-size
  "This is the threshold up to which we'll allow a duplicate Log4j2Plugins.dat
  file to be overwritten. It's arbitrary based on current versions being about
  3K in the log4j 1.2 bridge and about 20K in the log4j2 core file."
  5000)

(def ^:private ok-to-overwrite-idiotic-log4j2-file
  "Assume we can overwrite it until we hit a large version."
  (atom true))

(defn clash-strategy
  [filename]
  (cond
    (re-find #"data_readers.clj[sc]?$" filename)
    :merge-edn

    (re-find #"^META-INF/services/" filename)
    :concat-lines

    (= idiotic-log4j2-plugins-file filename)
    :log4j2-surgery

    :else
    :noop))

(defmulti clash (fn [filename _in _target]
                  (let [strategy (clash-strategy filename)]
                    (when-not *suppress-clash*
                      (prn {:warning "clashing jar item"
                            :path filename
                            :strategy strategy}))
                    strategy)))

(defmethod clash
  :merge-edn
  [_ in target]
  (let [;; read but do not close input stream
        f1 (edn/read (PushbackReader. (io/reader in)))
        ;; read and then close target since we will rewrite it
        f2 (with-open [r (PushbackReader. (Files/newBufferedReader target))]
             (edn/read r))]
    (with-open [w (Files/newBufferedWriter target open-opts)]
      (binding [*out* w]
        (prn (merge f1 f2))))))

(defmethod clash
  :concat-lines
  [_ in target]
  (let [f1 (line-seq (io/reader in))
        f2 (Files/readAllLines target)]
    (with-open [w (Files/newBufferedWriter target open-opts)]
      (binding [*out* w]
        (run! println (-> (vec f1)
                          (conj "\n")
                          (into f2)))))))

(defmethod clash
  :log4j2-surgery
  [filename ^InputStream in ^Path target]
  ;; we should also set the last mod date/time here but it isn't passed in
  ;; and I'm not going to make that change just to make this hack perfect!
  (if @ok-to-overwrite-idiotic-log4j2-file
    (do
      (when *debug*
        (println "overwriting" filename))
      (Files/copy in target ^"[Ljava.nio.file.CopyOption;" copy-opts)
      (when (< idiotic-log4j2-plugins-size (Files/size target))
        ;; we've copied a big enough file, stop overwriting it!
        (when *debug*
          (println "big enough -- no more copying!"))
        (reset! ok-to-overwrite-idiotic-log4j2-file false)))
    (when *debug*
      (println "ignoring" filename))))

(defmethod clash
  :default
  [_ _in _target]
  ;; do nothing, first file wins
  nil)

(def ^:private exclude-patterns
  "Filename patterns to exclude. These are checked with re-matches and
  should therefore be complete filename matches including any path."
  [#"project.clj"
   #"LICENSE"
   #"COPYRIGHT"
   #"\.keep"
   #".*\.pom$" #"module-info\.class$"
   #"(?i)META-INF/.*\.(?:MF|SF|RSA|DSA)"
   #"(?i)META-INF/(?:INDEX\.LIST|DEPENDENCIES|NOTICE|LICENSE)(?:\.txt)?"])

(defn excluded?
  [filename]
  (some #(re-matches % filename) exclude-patterns))

(defn copy!
  ;; filename drives strategy
  [filename ^InputStream in ^Path target last-mod]
  (if-not (excluded? filename)
    (if (Files/exists target (make-array LinkOption 0))
      (clash filename in target)
      (do
        (Files/copy in target ^"[Ljava.nio.file.CopyOption;" copy-opts)
        (when (= idiotic-log4j2-plugins-file filename)
          (when *debug*
            (println "copied" filename (Files/size target)))
          (when (< idiotic-log4j2-plugins-size (Files/size target))
            ;; we've copied a big enough file, stop overwriting it!
            (when *debug*
              (println "big enough -- no more copying!"))
            (reset! ok-to-overwrite-idiotic-log4j2-file false)))
        (when last-mod
          (Files/setLastModifiedTime target last-mod))))
    (when *debug*
      (prn {:excluded filename}))))

(defn consume-jar
  [^Path path f]
  (with-open [is (-> path
                     (Files/newInputStream (make-array OpenOption 0))
                     java.io.BufferedInputStream.
                     JarInputStream.)]
    (loop []
      (when-let [entry (.getNextJarEntry is)]
        (f is entry)
        (recur)))))

(defn classify
  [entry]
  (let [p (path entry)
        symlink-opts (make-array LinkOption 0)]
    (if (Files/exists p symlink-opts)
      (cond
        (Files/isDirectory p symlink-opts)
        :directory

        (and (Files/isRegularFile p symlink-opts)
             (re-find #"\.jar$" (.toString p)))
        :jar

        :else :unknown)
      :not-found)))

(defmulti copy-source*
  (fn [src _dest _options]
    (classify src)))

(defmethod copy-source*
  :jar
  [src dest options]
  (when-not (= :thin (:jar options))
    (when *verbose* (println src))
    (consume-jar (path src)
      (fn [inputstream ^JarEntry entry]
        (let [^String name (.getName entry)
              last-mod (.getLastModifiedTime entry)
              target (.resolve ^Path dest name)]
          (if (.isDirectory entry)
            (Files/createDirectories target (make-array FileAttribute 0))
            (do (Files/createDirectories (.getParent target) (make-array FileAttribute 0))
              (try
                (when (.startsWith name "META-INF/versions/")
                  (reset! multi-release? true))
                (copy! name inputstream target last-mod)
                (catch Throwable t
                  (prn {:error "unable to copy file"
                        :name name
                        :exception (class t)
                        :message (.getMessage t)})
                  (swap! errors inc))))))))))

(defn copy-directory
  [^Path src ^Path dest]
  (let [copy-dir
        (reify FileVisitor
          (visitFile [_ p attrs]
            (let [f (.relativize src p)
                  last-mod (Files/getLastModifiedTime p (make-array LinkOption 0))]
              (when *verbose* (println (str (.toString src) "/" (.toString f))))
              (with-open [is (Files/newInputStream p (make-array OpenOption 0))]
                (copy! (.toString f) is (.resolve dest (.toString f)) last-mod)))
            FileVisitResult/CONTINUE)
          (preVisitDirectory [_ p attrs]
            (Files/createDirectories (.resolve dest (.toString (.relativize src p)))
                                     (make-array FileAttribute 0))
            FileVisitResult/CONTINUE)
          (postVisitDirectory [_ p ioexc]
            (if ioexc (throw ioexc) FileVisitResult/CONTINUE))
          (visitFileFailed [_ p ioexc] (throw (ex-info "Visit File Failed" {:p p} ioexc))))]
    (Files/walkFileTree src visit-opts Integer/MAX_VALUE copy-dir)
    :ok))

(defmethod copy-source*
  :directory
  [src dest _options]
  (when *verbose* (println src))
  (copy-directory (path src) dest))

(defmethod copy-source*
  :not-found
  [src _dest _options]
  (prn {:warning "could not find classpath entry" :path src}))

(defmethod copy-source*
  :unknown
  [src _dest _options]
  (if (excluded? src)
    (when *debug* (prn {:excluded src}))
    (prn {:warning "ignoring unknown file type" :path src})))

(defn copy-source
  [src dest options]
  (copy-source* src dest options))

(defn the-classpath
  [classpath]
  (let [^String cp (or classpath (System/getProperty "java.class.path"))]
    (vec (.split cp (System/getProperty "path.separator")))))

(defn- manifest
  "Inserts manifest into uberjar"
  [^Path dest {:keys [#_jar main-class #_pom-file]}]
  (let [jdk         (str/replace (System/getProperty "java.version")
                                 #"_.*" "")
        build-now   (java.util.Date.)
        last-mod    (FileTime/fromMillis (.getTime build-now))
        manifest    (str "Manifest-Version: 1.0\n"
                         "Built-By: depstar\n"
                         "Build-Jdk: " jdk "\n"
                         (when @multi-release?
                           "Multi-Release: true\n")
                         (when main-class
                           (str "Main-Class: "
                                (str/replace main-class "-" "_")
                                "\n")))]
    (with-open [is (io/input-stream (.getBytes manifest))]
      (when *verbose*
        (print "\nGenerating META-INF/MANIFEST.MF:\n")
        (print manifest)
        (flush))
      (let [target (.resolve dest "META-INF/MANIFEST.MF")]
        (Files/createDirectories (.getParent target) (make-array FileAttribute 0))
        (copy! "MANIFEST.MF" is target last-mod)))))

(defn run
  [{:keys [dest jar suppress verbose]
    :or {jar :uber}
    :as options
    cp-opt :classpath}]
  (let [tmp-z-dir (Files/createTempDirectory "depstarz" (make-array FileAttribute 0))
        cp        (the-classpath cp-opt)
        dest-name (str/replace dest #"^.*[/\\]" "")
        jar-path  (.resolve tmp-z-dir ^String dest-name)
        jar-file  (java.net.URI. (str "jar:" (.toUri jar-path)))
        zip-opts  (doto (java.util.HashMap.)
                        (.put "create" "true")
                        (.put "encoding" "UTF-8"))]

    (with-open [zfs (FileSystems/newFileSystem jar-file zip-opts)]
      (let [tmp (.getPath zfs "/" (make-array String 0))]
        (reset! errors 0)
        (reset! multi-release? false)
        (binding [*debug* (env-prop "debug")
                  *suppress-clash* suppress
                  *verbose* verbose]
          (when *verbose* (println "Building" (str (name jar) "jar:") dest))
          (run! #(copy-source % tmp options) cp)
          (manifest tmp options))))
    (let [dest-path (path dest)
          parent (.getParent dest-path)]
      (when parent (.. parent toFile mkdirs))
      (Files/move jar-path dest-path copy-opts))
    (when (pos? @errors)
      (println "\nCompleted with errors!")
      (System/exit 1))))

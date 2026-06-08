;; peteches/packages/seerr.scm --- Seerr media request manager.
;;
;; Seerr is a Node.js/TypeScript application with a Next.js frontend and
;; Express backend.  Upstream uses pnpm, but this package avoids networked
;; package-manager activity and wires dependencies from Guix inputs instead.

(define-module (peteches packages seerr)
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix-science packages rstudio-node)
  #:use-module (guix build-system node)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (peteches packages seerr-deps))

(define-public seerr
  (package
    (name "seerr")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seerr-team/seerr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l18w45rm8csjjhizrz46px47pvrkbx1zxf98fi15pg0s7v22l59"))))
    (build-system node-build-system)
    (inputs
     (list node

           ;; Build-time TypeScript tooling expected by Next.js.
           node-typescript-5.9.3
           node-types-react-19.2.17
           node-types-node-25.9.2

           node-dr-pogodin-csurf-1.16.9
           node-fontsource-variable-inter-5.2.8
           node-formatjs-intl-4.1.4
           node-formatjs-intl-locale-5.3.2
           node-headlessui-react-1.7.19
           node-heroicons-react-2.2.0
           node-react-spring-web-10.0.3
           ;; @react-spring/web's ESM transitive deps — must be direct inputs so
           ;; they appear in node_modules/ for ESM resolution from build-tree.
           node-react-spring-animated-10.0.4
           node-react-spring-core-10.0.4
           node-react-spring-shared-10.0.4
           node-react-spring-rafz-10.0.4
           node-seerr-team-react-tailwindcss-datepicker-1.3.4
           node-supercharge-request-ip-1.2.0
           node-svgr-webpack-8.1.0
           node-tanem-react-nprogress-6.0.3
           node-ace-builds-1.43.6
           node-axios-1.15.0
           node-axios-rate-limit-1.9.0
           node-bcrypt-6.0.0
           node-bowser-2.14.1
           node-connect-typeorm-2.0.0
           node-cookie-parser-1.4.7
           node-copy-to-clipboard-4.0.0
           node-country-flag-icons-1.6.16
           node-cronstrue-3.14.0
           node-dns-caching-0.2.9
           node-email-templates-13.0.1
           node-express-5.2.1
           node-express-openapi-validator-5.6.2
           node-express-rate-limit-8.3.2
           node-express-session-1.19.0
           node-formik-2.4.9
           node-gravatar-url-4.0.1
           node-http-proxy-agent-7.0.2
           node-https-proxy-agent
           node-humanize-duration-3.33.2
           node-js-yaml-4.2.0
           node-lodash-4.18.1
           node-mime-4.1.0
           node-nanoid-5.1.7
           node-next-16.2.6

           ;; Native SWC binding used by Next.js.
           node-next-swc-linux-x64-gnu-16.2.6

           node-node-cache-5.1.2
           node-node-schedule-2.1.1
           node-nodemailer-8.0.5
           node-openpgp-6.3.0
           node-pg-8.20.0
           node-pug-3.0.4
           node-react-19.2.6
           node-react-ace-14.0.1
           node-react-animate-height-3.2.3
           node-react-aria-3.47.0
           node-react-dom-19.2.6
           node-react-hot-toast-2.6.0
           node-goober-2.1.19
           node-react-intersection-observer-10.0.3
           node-react-intl-7.1.14
           node-react-markdown-10.1.0
           node-react-popper-tooltip-4.4.2
           node-react-select-5.10.2
           node-react-transition-group-4.4.5
           node-react-truncate-markup-5.1.2
           node-react-use-clipboard-1.0.9
           node-reflect-metadata-0.2.2
           node-semver-7.7.4
           node-sharp-0.34.5
           node-sqlite3-5.1.7
           node-swagger-ui-express-5.0.1
           node-swr-2.4.1

           ;; Tailwind/PostCSS stack used by tailwind.config.js and Next's
           ;; Webpack CSS pipeline.
           ;;
           ;; Seerr's PostCSS config expects Tailwind 3-style use of
           ;; `tailwindcss' as the PostCSS plugin.  Do not use Tailwind 4 here
           ;; unless you also package @tailwindcss/postcss and patch the config.
           node-tailwindcss-3.4.17
           node-tailwindcss-forms-0.5.11
           node-tailwindcss-typography-0.5.16
           node-tailwindcss-aspect-ratio-0.4.2
           node-mini-svg-data-uri-1.4.4
           node-lodash-merge-4.6.2
           node-lodash-castarray-4.4.0
           node-lodash-isplainobject-4.0.6
           node-postcss-8.4.31
           node-autoprefixer-10.5.0
           node-dayjs-1.11.21

           ;; Tailwind 3 runtime dependency graph.  These must be direct Seerr
           ;; inputs because this package creates node_modules from direct inputs.
           node-postcss-selector-parser-6.1.2
           node-cssesc-3.0.0
           node-util-deprecate-1.0.2
           node-postcss-load-config-4.0.2
           node-alloc-quick-lru-5.2.0
           node-postcss-nested-6.2.0
           node-postcss-import-15.1.0
           node-normalize-path-3.0.0
           node-object-hash-3.0.0
           node-glob-parent-6.0.2
           node-postcss-js-4.1.0
           node-picocolors-1.1.1
           node-micromatch-4.0.8
           node-didyoumean-1.2.2
           node-lilconfig-3.1.3
           node-fast-glob-3.3.3
           node-chokidar-3.6.0
           node-sucrase-3.35.1
           node-resolve-1.22.12
           node-is-glob-4.0.3
           node-jiti-1.21.7
           node-dlv-1.1.3
           node-arg-5.0.2

           node-tailwind-merge-2.6.1
           node-typeorm-0.3.29
           node-ua-parser-js-2.0.9
           node-undici-8.1.0
           node-validator-13.15.35
           node-web-push-3.6.7
           node-wink-jaro-distance-2.0.0
           node-winston-3.19.0
           node-winston-daily-rotate-file-5.0.0
           node-xml2js-0.6.2
           node-yup-1.7.1
           node-zod-4.3.6))
    (arguments
     (list
      #:tests? #f
      #:modules
      '((guix build node-build-system)
        (guix build utils)
        (ice-9 ftw)
        (ice-9 match)
        (ice-9 popen)
        (ice-9 rdelim)
        (ice-9 regex)
        (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases

          (add-before 'patch-dependencies 'enter-package-json-directory
            (lambda _
              (unless (file-exists? "package.json")
                (let ((matches (find-files "." "^package\\.json$")))
                  (match matches
                    ((package-json)
                     (chdir (dirname package-json))
                     (format #t "changed directory to '~a'~%" (getcwd)))
                    (()
                     (error "could not find package.json after unpack"))
                    (_
                     (error "found multiple package.json files" matches)))))))

          (replace 'configure
            (lambda _
              (setenv "HOME" (getcwd))
              (setenv "XDG_CACHE_HOME" (string-append (getcwd) "/.cache"))
              (setenv "CYPRESS_INSTALL_BINARY" "0")
              (setenv "NEXT_TELEMETRY_DISABLED" "1")
              (setenv "NODE_ENV" "development")
              #t))

          ;; Let Next build the frontend without treating upstream frontend
          ;; typecheck issues as packaging failures.  The server is still
          ;; compiled explicitly with tsconfig.server.json.
          (add-after 'configure 'disable-next-typecheck
            (lambda _
              (let ((configs (filter file-exists?
                                     '("next.config.js"
                                       "next.config.mjs"
                                       "next.config.ts"))))
                (for-each
                 (lambda (file)
                   (format #t "patching Next.js typecheck config in ~a~%" file)
                   (substitute* file
                     (("typescript:[[:space:]]*\\{[^}]*\\}")
                      "typescript: { ignoreBuildErrors: true }")
                     (("eslint:[[:space:]]*\\{")
                      "typescript: { ignoreBuildErrors: true },\n  eslint: {")
                     (("const nextConfig([[:space:]]*:[^=]+)?[[:space:]]*=[[:space:]]*\\{")
                      "const nextConfig = {\n  typescript: { ignoreBuildErrors: true },")
                     (("module\\.exports[[:space:]]*=[[:space:]]*\\{")
                      "module.exports = {\n  typescript: { ignoreBuildErrors: true },")))
                 configs))
              #t))

          ;; Seerr's next.config provides an SVG→React-component rule for
          ;; Turbopack mode via turbopack.rules.  That block is ignored when
          ;; building with `next build --webpack'.  Without an explicit webpack
          ;; configuration function, Next.js' default webpack treats .svg imports
          ;; as static assets (URL-string or image-object exports), causing React
          ;; to throw "Element type is invalid: got: object" at runtime.
          (add-after 'disable-next-typecheck 'add-webpack-svgr-config
            (lambda _
              (let ((configs (filter file-exists?
                                     '("next.config.js"
                                       "next.config.mjs"
                                       "next.config.ts"))))
                (for-each
                 (lambda (file)
                   (format #t "adding webpack SVG/SVGR rule to ~a~%" file)
                   (substitute* file
                     ;; The transpilePackages line is unique and unaffected by
                     ;; the disable-next-typecheck patch, making it a stable
                     ;; insertion point.
                     ;; SVGO (enabled by default) runs removeMetadata which
                     ;; strips the <metadata> block from SVGs.  Without it,
                     ;; SVGs like emby-icon-only.svg that contain
                     ;; namespace-qualified elements (rdf:RDF, cc:Work, dc:type)
                     ;; cause Babel to throw "Namespace tags are not supported".
                     ;; The css-tree/csso version conflict previously requiring
                     ;; svgo:false is resolved by the fix-direct-node-module-links
                     ;; phase replacing the bundled css-tree@2.3.1 with 2.2.1.
                     (("transpilePackages:[[:space:]]*\\['country-flag-icons'\\],")
                      (string-append
                       "transpilePackages: ['country-flag-icons'],\n"
                       "  webpack: (config) => {\n"
                       "    config.module.rules.unshift({\n"
                       "      test: /\\.svg$/,\n"
                       "      issuer: /\\.[jt]sx?$/,\n"
                       "      use: ['@svgr/webpack'],\n"
                       "    });\n"
                       "    return config;\n"
                       "  },"))))
                 configs))
              #t))

          ;; Link Guix-provided node modules into the build tree.
          ;;
          ;; Most packages can be symlinked from the store.  The listed writable
          ;; packages are copied because we need to mutate their package
          ;; directories during the build, or because their own require() calls
          ;; should resolve through the build-tree node_modules.
          (add-after 'add-webpack-svgr-config 'link-node-inputs
            (lambda* (#:key inputs #:allow-other-keys)
              (define node-bin
                (search-input-file inputs "/bin/node"))

              (define writable-node-packages
                '("next"
                  "@next/swc-linux-x64-gnu"
                  "wink-jaro-distance"
                  "lodash"
                  "bcrypt"
                  "email-templates"
                  "nodemailer"
                  "humanize-duration"
                  "validator"
                  "ua-parser-js"
                  "bowser"
                  "web-push"
                  "autoprefixer"
                  ;; tailwindcss must be copied (not symlinked) so that Node.js
                  ;; resolves its transitive dependencies through the build-tree
                  ;; node_modules rather than walking up from the store path.
                  "tailwindcss"
                  ;; swr uses ESM imports which bypass NODE_PATH; must be writable
                  ;; so Node.js resolves 'react' from the build-tree node_modules.
                  "swr"
                  ;; react-hot-toast/dist/index.mjs uses ESM; must be in build tree
                  ;; so its 'import react' resolves from node_modules/ not the store.
                  "react-hot-toast"
                  ;; @react-spring/* use .modern.mjs ESM; all must be writable so
                  ;; their 'import react' resolves from build-tree node_modules/.
                  "@react-spring/web"
                  "@react-spring/animated"
                  "@react-spring/core"
                  "@react-spring/shared"
                  ;; @svgr/webpack bundles css-tree@2.3.1 alongside
                  ;; csso@5.0.5, but csso was built against css-tree@2.2.1.
                  ;; Node.js finds 2.3.1 first and csso crashes on init.
                  ;; Must be writable so fix-direct-node-module-links can
                  ;; replace the 2.3.1 directory with a symlink to 2.2.1.
                  "@svgr/webpack"))

              (define (writable-node-package? module-name)
                (member module-name writable-node-packages))

              (define (node-module-directories input)
                (let ((root (string-append input "/lib/node_modules")))
                  (if (file-exists? root)
                      (filter
                       (lambda (dir)
                         ;; Only accept top-level packages (root/pkg) or scoped
                         ;; packages (root/@org/pkg).  Deep recursion picks up
                         ;; packages bundled inside dist/compiled/ trees, which
                         ;; then shadow the real Guix-packaged modules.
                         (let* ((rel (substring dir (1+ (string-length root))))
                                (parts (filter (lambda (s) (not (string-null? s)))
                                               (string-split rel #\/))))
                           (or (= (length parts) 1)
                               (and (= (length parts) 2)
                                    (string-prefix? "@" (car parts))))))
                       (find-files root
                                   (lambda (file stat)
                                     (and (eq? 'directory (stat:type stat))
                                          (file-exists?
                                           (string-append file "/package.json"))))
                                   #:directories? #t))
                      '())))

              (define (read-package-name package-json)
                ;; Use node to parse the JSON so that minified package.json
                ;; files with "name" appearing in nested objects (e.g. inside
                ;; a contributors array) before the top-level "name" field are
                ;; handled correctly.
                (let* ((port (open-pipe* OPEN_READ
                                         node-bin
                                         "-e"
                                         (string-append
                                          "try{const p=JSON.parse("
                                          "require('fs').readFileSync("
                                          (format #f "~s" package-json)
                                          ",'utf8'));"
                                          "process.stdout.write(p.name||'')"
                                          "}catch(e){process.exit(0)}")))
                       (name (read-line port)))
                  (close-pipe port)
                  (and (not (eof-object? name))
                       (not (string-null? name))
                       name)))

              (define (link-package directory)
                (let* ((package-json (string-append directory "/package.json"))
                       (module-name  (read-package-name package-json)))
                  (when module-name
                    (let ((target (string-append "node_modules/" module-name)))
                      (mkdir-p (dirname target))
                      (unless (file-exists? target)
                        (if (writable-node-package? module-name)
                            (begin
                              (format #t "copying writable node module ~a -> ~a~%"
                                      module-name directory)
                              (copy-recursively directory target)
                              ;; Store files/dirs are read-only (mode 444/555).
                              ;; Make the copy writable so later phases can
                              ;; add files (e.g. index.d.ts) inside it.
                              (for-each (lambda (f)
                                          (let ((s (lstat f)))
                                            (unless (eq? 'symlink (stat:type s))
                                              (chmod f (logior (stat:mode s)
                                                               #o200)))))
                                        (find-files target #:directories? #t)))
                            (begin
                              (format #t "linking node module ~a -> ~a~%"
                                      module-name directory)
                              (symlink directory target))))))))

              (mkdir-p "node_modules")
              (for-each
               (lambda (input)
                 (for-each link-package
                           (node-module-directories input)))
               (map cdr inputs))
              #t))

          ;; Patch exports in bundled packages that ship inside copied writable
          ;; modules but whose exports field predates the subpath aliases that
          ;; newer consumers expect.
          (add-after 'link-node-inputs 'patch-bundled-package-exports
            (lambda* (#:key inputs #:allow-other-keys)
              (define node-bin (search-input-file inputs "/bin/node"))

              ;; entities ≤4.x exports "./lib/decode.js" but newer consumers
              ;; (e.g. htmlparser2 bundled in email-templates) require the
              ;; short alias "./decode".  Patch any bundled copy we find.
              (for-each
               (lambda (pkg-json)
                 (when (file-exists? pkg-json)
                   (chmod pkg-json #o644)
                   (invoke node-bin "-e"
                           (string-append
                            "const fs=require('fs');"
                            "const f=" (format #f "~s" pkg-json) ";"
                            "const p=JSON.parse(fs.readFileSync(f,'utf8'));"
                            "let changed=false;"
                            "if(p.exports&&!p.exports['./decode']&&p.exports['./lib/decode.js']){"
                            "  p.exports['./decode']=p.exports['./lib/decode.js'];"
                            "  changed=true;"
                            "}"
                            "if(p.exports&&!p.exports['./escape']&&p.exports['./lib/escape.js']){"
                            "  p.exports['./escape']=p.exports['./lib/escape.js'];"
                            "  changed=true;"
                            "}"
                            "if(changed){fs.writeFileSync(f,JSON.stringify(p));}"))))
               (find-files "node_modules"
                           (lambda (path stat)
                             (and (string-suffix? "/entities/package.json" path)
                                  (eq? 'regular (stat:type stat))))))
              #t))

          ;; Add small local declaration files for JS packages that do not ship
          ;; declarations and for which we do not yet have @types packages.
          (add-after 'patch-bundled-package-exports 'add-local-type-declarations
            (lambda _
              (define (add-types-field package-json)
                (substitute* package-json
                  (("\"types\"[[:space:]]*:[[:space:]]*\"[^\"]+\"")
                   "\"types\": \"index.d.ts\"")
                  (("\"main\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" all main)
                   (string-append all ",\n  \"types\": \"index.d.ts\""))))

              ;; wink-jaro-distance.
              (let* ((module-dir "node_modules/wink-jaro-distance")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
declare namespace jaro {
  interface Result {
    similarity: number;
    distance?: number;
  }
}

declare function jaro(
  source: string,
  target: string,
  options?: Record<string, unknown>
): jaro.Result;

export = jaro;
" port)))
                  (add-types-field package-json)))

              ;; lodash, plus lodash/orderBy subpath.
              (let* ((module-dir "node_modules/lodash")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export function sortBy<T>(
  collection: T[],
  iteratees?: string | string[] | ((value: T) => unknown)
): T[];

export function uniq<T>(array: T[]): T[];

export function uniqBy<T>(
  array: T[],
  iteratee: string | ((value: T) => unknown)
): T[];

export function uniqWith<T>(
  array: T[],
  comparator: (a: T, b: T) => boolean
): T[];

export function groupBy<T>(
  collection: T[],
  iteratee: string | ((value: T) => unknown)
): Record<string, T[]>;

export function keyBy<T>(
  collection: T[],
  iteratee: string | ((value: T) => unknown)
): Record<string, T>;

export function isEqual(value: unknown, other: unknown): boolean;
export function isNumber(value: unknown): value is number;
export function isString(value: unknown): value is string;
export function isUndefined(value: unknown): value is undefined;

export function merge<TObject, TSource>(
  object: TObject,
  source: TSource
): TObject & TSource;

export function mergeWith<TObject, TSource1, TSource2>(
  object: TObject,
  source1: TSource1,
  source2: TSource2,
  customizer: (
    objValue: unknown,
    srcValue: unknown,
    key?: string | number | symbol,
    object?: unknown,
    source?: unknown,
    stack?: unknown
  ) => unknown
): TObject & TSource1 & TSource2;

export function mergeWith<TObject>(
  object: TObject,
  ...sourcesAndCustomizer: any[]
): TObject;

export function orderBy<T>(
  collection: T[],
  iteratees?: string | string[] | ((value: T) => unknown),
  orders?: Array<'asc' | 'desc'> | 'asc' | 'desc'
): T[];

export function debounce<T extends (...args: any[]) => any>(
  func: T,
  wait?: number,
  options?: Record<string, unknown>
): T & { cancel(): void; flush(): ReturnType<T> };

export function truncate(
  string?: string,
  options?: {
    length?: number;
    omission?: string;
    separator?: string | RegExp;
  }
): string;
" port)))

                  (call-with-output-file
                      (string-append module-dir "/orderBy.d.ts")
                    (lambda (port)
                      (display "\
import { orderBy } from './index';
export = orderBy;
" port)))

                  (add-types-field package-json)))

              ;; bcrypt.
              (let* ((module-dir "node_modules/bcrypt")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export function hash(
  data: string | Buffer,
  saltOrRounds: string | number
): Promise<string>;

export function hashSync(
  data: string | Buffer,
  saltOrRounds: string | number
): string;

export function compare(
  data: string | Buffer,
  encrypted: string
): Promise<boolean>;

export function compareSync(
  data: string | Buffer,
  encrypted: string
): boolean;

export function genSalt(
  rounds?: number
): Promise<string>;

export function genSaltSync(
  rounds?: number
): string;

declare const bcrypt: {
  hash: typeof hash;
  hashSync: typeof hashSync;
  compare: typeof compare;
  compareSync: typeof compareSync;
  genSalt: typeof genSalt;
  genSaltSync: typeof genSaltSync;
};

export default bcrypt;
" port)))
                  (add-types-field package-json)))

              ;; email-templates.
              (let* ((module-dir "node_modules/email-templates")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export interface PreparedEmailSendOptions {
  template?: string;
  message?: Record<string, unknown>;
  locals?: Record<string, unknown>;
  [key: string]: unknown;
}

export default class Email {
  constructor(options?: unknown);
  send(options: PreparedEmailSendOptions): Promise<unknown>;
}

export class PreparedEmail {
  constructor(options?: unknown);
  send(options: PreparedEmailSendOptions): Promise<unknown>;
}
" port)))
                  (add-types-field package-json)))

              ;; nodemailer.
              (let* ((module-dir "node_modules/nodemailer")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export interface Transporter {
  sendMail(message: Record<string, unknown>): Promise<unknown>;
  use(step: string, plugin: unknown): void;
}

declare const nodemailer: {
  createTransport(options?: unknown): Transporter;
};

export default nodemailer;
" port)))
                  (add-types-field package-json)))

              ;; humanize-duration.
              (let* ((module-dir "node_modules/humanize-duration")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
declare function humanizeDuration(
  duration: number,
  options?: Record<string, unknown>
): string;

export = humanizeDuration;
" port)))
                  (add-types-field package-json)))

              ;; validator.
              (let* ((module-dir "node_modules/validator")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export function isEmail(value: string, options?: unknown): boolean;
export function isURL(value: string, options?: unknown): boolean;
export function isIP(value: string, version?: string | number): boolean;

declare const validator: {
  isEmail: typeof isEmail;
  isURL: typeof isURL;
  isIP: typeof isIP;
};

export default validator;
" port)))
                  (add-types-field package-json)))

              ;; ua-parser-js.
              (let* ((module-dir "node_modules/ua-parser-js")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export class UAParser {
  constructor(userAgent?: string);
  getResult(): Record<string, unknown>;
  getBrowser(): Record<string, unknown>;
  getOS(): Record<string, unknown>;
  getDevice(): Record<string, unknown>;
}

export default UAParser;
" port)))
                  (add-types-field package-json)))

              ;; bowser.
              (let* ((module-dir "node_modules/bowser")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
declare const Bowser: {
  parse(userAgent: string): Record<string, unknown>;
  getParser(userAgent: string): unknown;
};

export default Bowser;
" port)))
                  (add-types-field package-json)))

              ;; web-push.
              (let* ((module-dir "node_modules/web-push")
                     (types-file (string-append module-dir "/index.d.ts"))
                     (package-json (string-append module-dir "/package.json")))
                (when (file-exists? module-dir)
                  (call-with-output-file types-file
                    (lambda (port)
                      (display "\
export function setVapidDetails(
  subject: string,
  publicKey: string,
  privateKey: string
): void;

export function sendNotification(
  subscription: unknown,
  payload?: string | Buffer,
  options?: Record<string, unknown>
): Promise<unknown>;

export function generateVAPIDKeys(): {
  publicKey: string;
  privateKey: string;
};
" port)))
                  (add-types-field package-json)))

              #t))

          ;; Force important direct imports/types to exist at the root of
          ;; node_modules.  Do not force-link packages whose writable copies
          ;; were patched above with local index.d.ts files.
          (add-after 'add-local-type-declarations 'fix-direct-node-module-links
            (lambda* (#:key inputs #:allow-other-keys)
              (define (force-link target source)
                (unless (file-exists? source)
                  (error "missing source module directory" source))
                (mkdir-p (dirname target))
                (when (file-exists? target)
                  (delete-file-recursively target))
                (format #t "forcing node module ~a -> ~a~%" target source)
                (symlink source target))

              ;; postcss, required by autoprefixer during Next/Webpack CSS
              ;; processing.
              (let* ((input
                      (or (assoc-ref inputs "node-postcss")
                          (assoc-ref inputs "node-postcss-8.5.6")
                          (assoc-ref inputs "node-postcss-8.4.49")
                          (assoc-ref inputs "node-postcss-8.4.31")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/postcss"))))
                (when input
                  (force-link "node_modules/postcss" source)))

              ;; node-cache.
              (let* ((input
                      (or (assoc-ref inputs "node-node-cache")
                          (assoc-ref inputs "node-node-cache-5.1.2")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/node-cache"))))
                (unless input
                  (error "could not find node-cache input; check the input label"))
                (force-link "node_modules/node-cache" source))

              ;; @types/node.
              (let* ((input
                      (or (assoc-ref inputs "node-types-node")
                          (assoc-ref inputs "node-types-node-25.9.2")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/@types/node"))))
                (unless input
                  (error "could not find @types/node input; check the input label"))
                (force-link "node_modules/@types/node" source))

              ;; @types/react.
              (let* ((input
                      (or (assoc-ref inputs "node-types-react")
                          (assoc-ref inputs "node-types-react-19.2.17")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/@types/react"))))
                (when input
                  (force-link "node_modules/@types/react" source)))

              ;; zod.
              (let* ((input
                      (or (assoc-ref inputs "node-zod")
                          (assoc-ref inputs "node-zod-4.3.6")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/zod"))))
                (unless input
                  (error "could not find zod input; check the input label"))
                (force-link "node_modules/zod" source))

              ;; @tailwindcss/forms.
              (let* ((input
                      (or (assoc-ref inputs "node-tailwindcss-forms")
                          (assoc-ref inputs "node-tailwindcss-forms-0.5.10")
                          (assoc-ref inputs "node-tailwindcss-forms-0.5.11")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/@tailwindcss/forms"))))
                (when input
                  (force-link "node_modules/@tailwindcss/forms" source)))

              ;; @tailwindcss/typography.
              (let* ((input
                      (or (assoc-ref inputs "node-tailwindcss-typography")
                          (assoc-ref inputs "node-tailwindcss-typography-0.5.16")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/@tailwindcss/typography"))))
                (when input
                  (force-link "node_modules/@tailwindcss/typography" source)))

              ;; @tailwindcss/aspect-ratio.
              (let* ((input
                      (or (assoc-ref inputs "node-tailwindcss-aspect-ratio")
                          (assoc-ref inputs "node-tailwindcss-aspect-ratio-0.4.2")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/@tailwindcss/aspect-ratio"))))
                (when input
                  (force-link "node_modules/@tailwindcss/aspect-ratio" source)))

              ;; Do not force-link autoprefixer here.  It is intentionally
              ;; copied into node_modules by link-node-inputs because it needs
              ;; to resolve postcss from the build-tree node_modules.

              ;; dayjs, used by @seerr-team/react-tailwindcss-datepicker,
              ;; including locale subpaths such as dayjs/locale/af.
              (let* ((input
                      (or (assoc-ref inputs "node-dayjs")
                          (assoc-ref inputs "node-dayjs-1.11.13")
                          (assoc-ref inputs "node-dayjs-1.11.19")
                          (assoc-ref inputs "node-dayjs-1.11.21")))
                     (source
                      (and input
                           (string-append input
                                          "/lib/node_modules/dayjs"))))
                (when input
                  (force-link "node_modules/dayjs" source)))

              ;; @svgr/webpack bundles css-tree@2.3.1 next to csso@5.0.5, but
              ;; csso was built (as a Guix package) against css-tree@2.2.1.
              ;; Node.js module resolution finds 2.3.1 first; csso crashes at
              ;; module-init time with "Missed `structure' field".  Replace the
              ;; bundled 2.3.1 directory with a symlink to the 2.2.1 store path
              ;; that is recorded in csso's own package.json dependencies field.
              (let* ((node-bin  (search-input-file inputs "/bin/node"))
                     (svgr-nm   "node_modules/@svgr/webpack/node_modules")
                     (ct-dir    (string-append svgr-nm "/css-tree"))
                     (csso-pkg  (string-append svgr-nm "/csso/package.json")))
                (when (and (file-exists? ct-dir)
                           (file-exists? csso-pkg))
                  (let* ((port    (open-pipe* OPEN_READ
                                             node-bin "-e"
                                             (string-append
                                              "try{const p=JSON.parse("
                                              "require('fs').readFileSync("
                                              (format #f "~s" csso-pkg)
                                              ",'utf8'));"
                                              "process.stdout.write("
                                              "(p.dependencies||{})"
                                              "['css-tree']||'')"
                                              "}catch(e){process.exit(0)}")))
                         (ct-path (let ((l (read-line port)))
                                    (close-pipe port)
                                    (if (eof-object? l) "" l))))
                    (when (and (not (string-null? ct-path))
                               (file-exists? ct-path))
                      (format #t "replacing @svgr/webpack bundled css-tree ~
with ~a~%" ct-path)
                      (delete-file-recursively ct-dir)
                      (symlink ct-path ct-dir)))))

              #t))

          ;; Link binaries from actual node package inputs only.
          (add-after 'fix-direct-node-module-links 'link-node-binaries
            (lambda* (#:key inputs #:allow-other-keys)
              (define (directory-entries directory)
                (filter
                 (lambda (entry)
                   (not (member entry '("." ".."))))
                 (scandir directory)))

              (define (link-bin-directory input)
                (let ((bin (string-append input "/bin")))
                  (when (file-exists? bin)
                    (for-each
                     (lambda (entry)
                       (let ((source (string-append bin "/" entry))
                             (target (string-append "node_modules/.bin/" entry)))
                         (when (and (not (file-exists? target))
                                    (not (file-is-directory? source)))
                           (format #t "linking node binary ~a -> ~a~%"
                                   entry source)
                           (symlink source target))))
                     (directory-entries bin)))))

              (mkdir-p "node_modules/.bin")
              (for-each
               (lambda (input)
                 (when (file-exists? (string-append input "/lib/node_modules"))
                   (link-bin-directory input)))
               (map cdr inputs))
              #t))

          ;; Build the frontend and backend.
          (replace 'build
            (lambda _
              (setenv "NODE_ENV" "development")
              (setenv "NEXT_TELEMETRY_DISABLED" "1")
              (setenv "NEXT_SKIP_NATIVE_POSTINSTALL" "1")
              ;; Symlinked packages resolve ESM imports from the symlink location
              ;; (build-tree node_modules/) instead of the real store path, where
              ;; peer deps like 'react' are not accessible.
              (setenv "NODE_OPTIONS" "--preserve-symlinks")

              (unless (file-exists? "node_modules/@next/swc-linux-x64-gnu")
                (error "missing @next/swc-linux-x64-gnu in node_modules"))

              ;; Use Webpack rather than Turbopack.  Seerr's build uses
              ;; Webpack/SVGR-shaped configuration, and Turbopack currently
              ;; trips over that graph here.
              (invoke "node"
                      "node_modules/next/dist/bin/next"
                      "build"
                      "--webpack")

              (unless (file-exists? "node_modules/.bin/tsc")
                (error "missing TypeScript compiler" "node_modules/.bin/tsc"))

              ;; Compile the Express server.  --noCheck skips type-checking (a
              ;; TypeScript 5.5+ flag) so missing @types dev-deps don't block
              ;; the build; the JS output is still produced correctly.
              (invoke "node_modules/.bin/tsc"
                      "--project" "server/tsconfig.json"
                      "--noCheck")

              ;; Rewrite @server/* path aliases left verbatim by tsc.
              ;; tsc-alias is not packaged; replicate its logic in Guile.
              (for-each
               (lambda (js-file)
                 (let* ((rel    (string-drop js-file 5))
                        (dir    (dirname rel))
                        (depth  (if (member dir '("" "."))
                                    0
                                    (length (filter (compose not string-null?)
                                                    (string-split dir #\/)))))
                        (prefix (if (zero? depth)
                                    "./"
                                    (string-join (make-list depth "..") "/" 'suffix))))
                   (substitute* js-file
                     (("\"@server/") (string-append "\"" prefix)))))
               (find-files "dist" "\\.js$"))

              (when (file-exists? "server/templates")
                (copy-recursively "server/templates" "dist/templates"))
              (let ((locale-src "server/i18n/locale"))
                (when (file-exists? locale-src)
                  (mkdir-p "dist/i18n/locale")
                  (copy-recursively locale-src "dist/i18n/locale")))))

          ;; Install the app broadly while iterating.  Once this builds, this
          ;; can be tightened to only runtime files.
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (app-dir (string-append out "/lib/node_modules/seerr")))
                (mkdir-p app-dir)
                (copy-recursively "." app-dir)
                #t)))

          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (app-dir  (string-append out "/lib/node_modules/seerr"))
                     (node-bin (search-input-file inputs "/bin/node"))
                     (wrapper  (string-append out "/bin/seerr")))

                (mkdir-p (string-append out "/bin"))

                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                            "\
#!/bin/sh
export NODE_ENV=production
export NEXT_TELEMETRY_DISABLED=1

# --preserve-symlinks makes symlinked node_modules/ entries resolve their ESM
# peer dependencies (like react) through the symlink location (the app's
# node_modules/) instead of the real store path, where peers are absent.
# ESM imports do not fall back to NODE_PATH, so this flag is required for any
# symlinked package that uses ESM.  Mirrors the build-time NODE_OPTIONS setting.
export NODE_OPTIONS=\"--preserve-symlinks${NODE_OPTIONS:+ $NODE_OPTIONS}\"

# CONFIG_DIRECTORY and PORT are expected to be provided by the Shepherd service.
# NODE_PATH must point to the app's own node_modules/ so that symlinked
# packages (e.g. winston-daily-rotate-file) can resolve their peer deps
# (e.g. winston) via NODE_PATH when CJS resolution walks up from the real
# store path rather than the symlink location.
export NODE_PATH=\"~a/node_modules${NODE_PATH:+:$NODE_PATH}\"

# Next.js resolves .next/ relative to process.cwd(), so we must run from
# the app directory.
cd ~a

exec ~a dist/index.js \"$@\"
"
                            app-dir
                            app-dir
                            node-bin)))

                (chmod wrapper #o755)
                #t))))))

    (home-page "https://docs.seerr.dev/")
    (synopsis "Media request manager for Jellyfin, Sonarr, and Radarr")
    (description
     "Seerr is a request management and media discovery tool that integrates
with Jellyfin, Sonarr, and Radarr to allow users to request movies and TV
shows from a self-hosted media server.  It is the merged successor to
Overseerr and Jellyseerr.")
    (license expat)))

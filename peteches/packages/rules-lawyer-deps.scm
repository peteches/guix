(define-module (peteches packages rules-lawyer-deps)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

;;; Commentary:
;;;
;;; The Node.js dependency closure of the rules-lawyer Discord bot, pinned to
;;; the exact versions in discord/package-lock.json (discord.js 14.27.0 and
;;; its 23 transitive dependencies).
;;;
;;; Each package is a plain url-fetch of the npm tarball, extracted verbatim to
;;; lib/node_modules/<npm-name>/ with gnu-build-system (configure/build/check
;;; deleted, install replaced by a copy). No npm is run: these are leaf
;;; packages whose files the culminating rules-lawyer package copies into its
;;; own node_modules. The nested @discordjs/collection 2.1.1 (a second version
;;; of the same name) is packaged as its own store item, so both versions are
;;; available to the culminating package.

(define (make-node-dep-package guix-name version npm-name uri sha256-b32 lic)
  "Build a store item holding the npm package NPM-NAME at lib/node_modules/NPM-NAME."
  (package
    (name guix-name)
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri uri)
       (sha256 (base32 sha256-b32))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (replace 'install
            (lambda _
              (let ((dest (string-append #$output "/lib/node_modules/"
                                         #$npm-name)))
                (copy-recursively "." dest)))))))
    (home-page "https://registry.npmjs.org/")
    (synopsis (string-append "Node.js dependency " npm-name))
    (description "Node.js dependency for the rules-lawyer Discord bot.")
    (license lic)))

(define-public node-discordjs-builders-1.14.1
  (make-node-dep-package "node-discordjs-builders" "1.14.1" "@discordjs/builders"
    "https://registry.npmjs.org/@discordjs/builders/-/builders-1.14.1.tgz"
    "07yn7pn2vw4q0y5vv4bliw0xrf3zc7yqbhhx4vq343z2yvm5hd9y"
    license:asl2.0))

(define-public node-discordjs-collection-1.5.3
  (make-node-dep-package "node-discordjs-collection" "1.5.3" "@discordjs/collection"
    "https://registry.npmjs.org/@discordjs/collection/-/collection-1.5.3.tgz"
    "030qnlxhzbmkrycs5w9l8d32pw9c7lgh5hm46pnpfg8v1m7bzb6w"
    license:asl2.0))

(define-public node-discordjs-formatters-0.6.2
  (make-node-dep-package "node-discordjs-formatters" "0.6.2" "@discordjs/formatters"
    "https://registry.npmjs.org/@discordjs/formatters/-/formatters-0.6.2.tgz"
    "0n6w5vs2hr6dj3qv4a15diyhhccccspsbihi7ql7vi9cnqjn9c89"
    license:asl2.0))

(define-public node-discordjs-rest-2.6.3
  (make-node-dep-package "node-discordjs-rest" "2.6.3" "@discordjs/rest"
    "https://registry.npmjs.org/@discordjs/rest/-/rest-2.6.3.tgz"
    "14wb7y58bp1h7cc9v2vmihsc780d6s32nqdma7jrplkdqsyw136j"
    license:asl2.0))

(define-public node-discordjs-collection-2.1.1
  (make-node-dep-package "node-discordjs-collection" "2.1.1" "@discordjs/collection"
    "https://registry.npmjs.org/@discordjs/collection/-/collection-2.1.1.tgz"
    "03r01hzsqgsfm7qag6xwh0w4s4qz08c5sh1bs8xwjkfp0a4sq6rk"
    license:asl2.0))

(define-public node-discordjs-util-1.2.0
  (make-node-dep-package "node-discordjs-util" "1.2.0" "@discordjs/util"
    "https://registry.npmjs.org/@discordjs/util/-/util-1.2.0.tgz"
    "0zr2yi02s9vs6532r59v6558xq8j4pswqrc2w51gdvl1vb1yrn2w"
    license:asl2.0))

(define-public node-discordjs-ws-1.2.3
  (make-node-dep-package "node-discordjs-ws" "1.2.3" "@discordjs/ws"
    "https://registry.npmjs.org/@discordjs/ws/-/ws-1.2.3.tgz"
    "1sqzvq461pl4r09wq9diik53mp0f735s1g01jn9qy4zd88vhv27s"
    license:asl2.0))

(define-public node-sapphire-async-queue-1.5.5
  (make-node-dep-package "node-sapphire-async-queue" "1.5.5" "@sapphire/async-queue"
    "https://registry.npmjs.org/@sapphire/async-queue/-/async-queue-1.5.5.tgz"
    "0apyzkyzhn77riy2zq9vl1zwdzfb79s7wb4rjcv6cnh03lilbj5g"
    license:expat))

(define-public node-sapphire-shapeshift-4.0.0
  (make-node-dep-package "node-sapphire-shapeshift" "4.0.0" "@sapphire/shapeshift"
    "https://registry.npmjs.org/@sapphire/shapeshift/-/shapeshift-4.0.0.tgz"
    "02ynwjy3z6d3xd020aa0azjm2c32lqsq09d35gmvgw710r9brdbx"
    license:expat))

(define-public node-sapphire-snowflake-3.5.5
  (make-node-dep-package "node-sapphire-snowflake" "3.5.5" "@sapphire/snowflake"
    "https://registry.npmjs.org/@sapphire/snowflake/-/snowflake-3.5.5.tgz"
    "180z210zq5gvsvqn78zy7kigi7ldk88nwfivw0qk1c62kpgwyhha"
    license:expat))

(define-public node-types-node-26.6.5
  (make-node-dep-package "node-types-node" "26.6.5" "@types/node"
    "https://registry.npmjs.org/@types/node/-/node-26.6.5.tgz"
    "1pyvr1d64x10rc7v68zfssf9sacn6fwachwhlxdals84hsn7vr74"
    license:expat))

(define-public node-types-ws-8.18.2
  (make-node-dep-package "node-types-ws" "8.18.2" "@types/ws"
    "https://registry.npmjs.org/@types/ws/-/ws-8.18.2.tgz"
    "1zl3vgw2jxx2zvs7q25iyx7gh3ygd65zccbvlj6zb5skvq246xwx"
    license:expat))

(define-public node-vladfrangu-async-event-emitter-2.4.7
  (make-node-dep-package "node-vladfrangu-async-event-emitter" "2.4.7"
    "@vladfrangu/async_event_emitter"
    "https://registry.npmjs.org/@vladfrangu/async_event_emitter/-/async_event_emitter-2.4.7.tgz"
    "0szgzfvh57cyv8xl5sx6nirmb921npa1z58kz0llbf6arw63abww"
    license:expat))

(define-public node-discord-api-types-0.38.57
  (make-node-dep-package "node-discord-api-types" "0.38.57" "discord-api-types"
    "https://registry.npmjs.org/discord-api-types/-/discord-api-types-0.38.57.tgz"
    "12lyi91c74f8pjjwxrcsdz73n67rmmkrz7wa787kbaxk9mcxbqqv"
    license:expat))

(define-public node-discordjs-14.27.0
  (make-node-dep-package "node-discordjs" "14.27.0" "discord.js"
    "https://registry.npmjs.org/discord.js/-/discord.js-14.27.0.tgz"
    "19qj744xvr0i40169glpvzy7qj33gff79lbzs60nqjajmdhfdpzk"
    license:asl2.0))

(define-public node-fast-deep-equal-3.1.3
  (make-node-dep-package "node-fast-deep-equal" "3.1.3" "fast-deep-equal"
    "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz"
    "13vvwib6za4zh7054n3fg86y127ig3jb0djqz31qsqr71yca06dh"
    license:expat))

(define-public node-lodash-4.18.1
  (make-node-dep-package "node-lodash" "4.18.1" "lodash"
    "https://registry.npmjs.org/lodash/-/lodash-4.18.1.tgz"
    "05mfj422yf8xfpaf9gyd3ph2vd83rwng39cwir1dgjc8lk144sb9"
    license:expat))

(define-public node-lodash-snakecase-4.1.1
  (make-node-dep-package "node-lodash-snakecase" "4.1.1" "lodash.snakecase"
    "https://registry.npmjs.org/lodash.snakecase/-/lodash.snakecase-4.1.1.tgz"
    "0b1c61fgsjiqk92p03aisgxdl1frxfpnr2avmwi9m943h1w2q0sk"
    license:expat))

(define-public node-magic-bytes-js-1.13.1
  (make-node-dep-package "node-magic-bytes-js" "1.13.1" "magic-bytes.js"
    "https://registry.npmjs.org/magic-bytes.js/-/magic-bytes.js-1.13.1.tgz"
    "1n6jyp1gk3773r5w8x13xf56a6waxrdwx1kvhjk9y6zc5vbv7alz"
    license:expat))

(define-public node-ts-mixer-6.0.4
  (make-node-dep-package "node-ts-mixer" "6.0.4" "ts-mixer"
    "https://registry.npmjs.org/ts-mixer/-/ts-mixer-6.0.4.tgz"
    "0daln53hnflhij9mn1xy22f4sisch8mm76syx657wd6g3g7yf7nj"
    license:expat))

(define-public node-tslib-2.8.1
  (make-node-dep-package "node-tslib" "2.8.1" "tslib"
    "https://registry.npmjs.org/tslib/-/tslib-2.8.1.tgz"
    "17hiw9pawyczkhsnhlq4k9dn3kq2l49nk5rlfn049bmbxvakbxk6"
    license:bsd-0))

(define-public node-undici-6.29.1
  (make-node-dep-package "node-undici" "6.29.1" "undici"
    "https://registry.npmjs.org/undici/-/undici-6.29.1.tgz"
    "0kx0f3kk2z7l6n1qad7xxha7l1ff2a89bhbiamwvhv6qnbhbii19"
    license:expat))

(define-public node-undici-types-8.9.0
  (make-node-dep-package "node-undici-types" "8.9.0" "undici-types"
    "https://registry.npmjs.org/undici-types/-/undici-types-8.9.0.tgz"
    "1ikq7qx4grfa4k5pg93snwd2lzlxh7mki2mc1sr2245zri8phn1s"
    license:expat))

(define-public node-ws-8.22.0
  (make-node-dep-package "node-ws" "8.22.0" "ws"
    "https://registry.npmjs.org/ws/-/ws-8.22.0.tgz"
    "1j5bwyagz7a0vk0j61ikkfq15ah10ppm0gviqvxfh771nac3g6ya"
    license:expat))
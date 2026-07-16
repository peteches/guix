;;; peteches/home/modules/syncthing.scm — syncs ~/area_51/org between machines.
;;;
;;; One folder ("org") shared sendreceive between nug and nyarlothotep.
;;; The GUI binds to 127.0.0.1:8384 only — reach it over an SSH tunnel, not
;;; the LAN.
;;;
;;; The device IDs are Syncthing public identities, not secrets.
;;;
;;; CAVEAT: `path' is computed with (getenv "HOME") at *module-load* time,
;;; i.e. in whatever environment runs `guix home reconfigure'.  That is fine
;;; for interactive use but would resolve wrongly if a build ever evaluated
;;; this module with a different HOME.  Everything else in this repo takes
;;; the home directory from the activation-time environment instead.

(define-module (peteches home modules syncthing)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu services)
  #:export (base-syncthing-service))

(define-public base-syncthing-service
  (service home-syncthing-service-type
	   (for-home
	    (syncthing-configuration
	     (config-file
	      (syncthing-config-file
	       (gui-address "127.0.0.1:8384")

	       (folders
		(list (syncthing-folder
		       (id "org")
		       (label "Org")
		       (path (string-append (getenv "HOME") "/area_51/org"))
		       (type 'sendreceive)
		       (paused? #f)
		       (devices
			(list
			 (syncthing-device
			  (name "nug")
			  (id "END7B5S-OJ73D3S-CCPWFE3-Q45OGLZ-BAGJ63I-CF4YBQ6-6QE2U36-YFLGZA2")
			  (auto-accept-folders? #t))
			 (syncthing-device
			  (name "nyarlothotep")
			  (id "NONO6A6-UEOXJWK-JI5TWRF-5NDMD6H-N3BDTWI-JKRNQ5D-PHO4SSW-UDTHFAL")
			  (auto-accept-folders? #t)))))))))))))

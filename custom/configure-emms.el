;; -*- lexical-binding: t -*-

;;* emms
;; (package-vc-install "https://git.savannah.gnu.org/git/emms.git")
(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native emms-info-exiftool
                            emms-info-metaflac))
(setq emms-player-mpv-parameters
      '("--quiet" "--really-quiet" "--no-audio-display"))


;;* mpvi.el
;; (package-vc-install "https://github.com/lorniu/mpvi")
;; https://github.com/lorniu/mpvi/blob/master/README-en.md


(provide 'configure-emms)

(asdf:defsystem "wernicke"
	:serial t
	:version "0.1"
	:author "Dylan Ball <arathnim@gmail.com>"
	:maintainer "Dylan Ball <arathnim@gmail.com>"
	:description "parsing framework"
	:depends-on (alexandria iterate anaphora)
	:components ((:file "wernicke")))

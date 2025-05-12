test:
	ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :oscl/test)" \
	        --eval "(rove:run :oscl/test :style :spec)"


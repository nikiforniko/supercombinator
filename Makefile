HEROKU_APP=gmachine-api

heroku_deploy: heroku_push heroku_release

heroku_push:
	heroku container:push -a $(HEROKU_APP) web

heroku_release:
	heroku container:release -a $(HEROKU_APP) web

heroku_log:
	heroku logs -a $(HEROKU_APP) --tail

heroku_restart:
	heroku restart -a $(HEROKU_APP)

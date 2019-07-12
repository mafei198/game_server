all:
	./cmd install

console:
	cd game_server && tools/start

deamon:
	./cmd start

stop:
	./cmd stop

attach:
	./cmd attach

test:
	cd game_server && tools/run_test

clean:
	cd game_server && ./rebar clean

compile:
	cd game_server && ./rebar compile

rails_console:
	cd app/server_engine && rails c

migrate:
	cd app/server_engine && bundle exec rake db:migrate
generate_api:
	cd app/server_engine && bundle exec rake generate_api
generate_config:
	cd app/server_engine && bundle exec rake generate_config
generate_db:
	cd app/server_engine && bundle exec rake generate_database_record
generate_record:
	cd app/server_engine && bundle exec rake generate_record
generate_error_code:
	cd app/server_engine && bundle exec rake generate_error_code

resetup:
	cd game_server && ./rebar clean
	cd game_server && rm -rf Mnesia.*
	cd app/server_engine && bundle exec rake db:drop:all
	./cmd install

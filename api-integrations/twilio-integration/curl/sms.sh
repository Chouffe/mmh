# Curl the server for replying to an incoming SMS
# curl --data "Body=Hello World" --data-urlencode "From=+123456789" http://localhost:8081/api/sms

# Ping <-> Pong
curl http://localhost:8081/api/ping

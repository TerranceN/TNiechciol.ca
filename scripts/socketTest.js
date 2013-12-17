window.onload = function() {
    var connection = {};

    console.log("Attemping connection...");
    connection = new WebSocket("ws://test.tniechciol.ca:8088");

    connection.onopen = function() {
        console.log("Connection accepted!!!");
    }

    connection.onmessage = function(e) {
        console.log('Server: ' + e.data)
    }
}

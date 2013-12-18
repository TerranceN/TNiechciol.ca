window.onload = function() {
    var connection = {};
    var content = document.getElementById("pageContent");
    var list = document.createElement("div");
    list.setAttribute("id", "connectedList");
    content.appendChild(list)

    var addNewConnection = function(playerId) {
        var test = document.getElementById("playerId" + playerId);
        if (test == null) {
            var elem = document.createElement("div");
            elem.setAttribute("id", "playerId" + playerId);
            elem.innerHTML = playerId
            list.appendChild(elem);
        }
    }

    var removeConnection = function(playerId) {
        var elem = document.getElementById("playerId" + playerId);
        if (elem != null) {
            list.removeChild(elem);
        }
    }

    console.log("Attemping connection...");
    connection = new WebSocket("ws://test.tniechciol.ca:8088");

    connection.onopen = function() {
        console.log("Connection accepted!!!");
    }

    connection.onmessage = function(e) {
        if (e.data.indexOf("Disconnect:") == 0) {
            removeConnection(e.data.substring(11));
        } else if (e.data.indexOf("Connection:") == 0) {
            addNewConnection(e.data.substring(11));
        } else if (e.data.indexOf("Message:") == 0) {
            console.log(e.data);
        } else {
            console.log('Unknown message: ' + e.data);
        }
    }
}

$(function (){
    var ws;
    function addStatus(text){
        var date = new Date();
        document.getElementById('status').innerHTML
    = document.getElementById('status').innerHTML
    + date + ": " + text + "<br/>";
    }

    function addSbbConnection(from, to) {
        var topic = from+":"+to;
        $("#webhooks").append("<li id="+topic+">"+from+" -> "+to+"</li>");
        ws.send("subscribe:"+topic);
    }


    function init(){
        $("#subscribe").click(function(e){
            var from = $("#from").val();
            var to = $("#to").val();
            addSbbConnection(from, to);
        });


        if ("MozWebSocket" in window) {
            WebSocket = MozWebSocket;
        }
        if ("WebSocket" in window) {
            // browser supports websockets
            ws = new WebSocket("ws://"+window.location.host+"/pubsub/sbb");
            ws.onopen = function() {
                // websocket is connected
                console.log("websocket connected!");
            };
            ws.onmessage = function (evt) {
                console.log(evt.data);
                var receivedMsg = jQuery.parseJSON(evt.data);
                topic = receivedMsg.topic;
                console.log(topic, receivedMsg.data);
                //$("#"+topic).append("+");
            };
            ws.onclose = function() {
                // websocket was closed
                console.log("websocket was closed");
            };
        } else {
            // browser does not support websockets
            console.log("sorry, your browser does not support websockets.");
        }
    }


    //
    //
    //
    //
    //
    //[
    //    {topic: "basel:zurich",
    //     data: [
    //
    //        {type: "arrival_delay",
    //         time: 100},
    //        {type: "departure_delay",
    //         time: 100},
    //        {type: "from_track_changed",
    //         track: 10},
    //        {type: "to_track_changed",
    //         track: 10},
    //
    //    ]}
    //
    //
    //]
    //
    //
    //
    //
    init();
});

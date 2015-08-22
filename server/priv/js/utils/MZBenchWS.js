class MZBenchWS {
    constructor() {
        this.wsSocket = undefined;
    }

    connect(url, callbacks) {
        let {onopen, onmessage} = callbacks;

        let location = window.location;
        let protocol = "https:" == location.protocol ? "wss:" : "ws:";
        let wsUrl = protocol + "//" + location.host + url;

        let socket = new WebSocket(wsUrl);

        if (onopen) {
            socket.onopen = (event) => { onopen(socket) };
        }

        socket.onclose = (event) => {
            this.wsSocket = undefined;
            setTimeout(() => this.connect(url, callbacks), 10000);
        };

        socket.onmessage = (event) => {
            let data = JSON.parse(event.data);
            if ("ping" == data) {
                socket.send(JSON.stringify("pong"));
            } else {
                if (onmessage) { onmessage(data, socket); }
            }
        };

        socket.onerror = (event) => console.log(event);

        window.onbeforeunload = () => {
            socket.onclose = () => {};
            socket.close()
        };

        this.wsSocket = socket;
    }

    send(data) {
        if (this.wsSocket && (this.wsSocket.readyState == WebSocket.OPEN)) {
            this.wsSocket.send(JSON.stringify(data));
        } else {
            console.log("WebSocket is not connected");
        }
    }

    close() {
        this.wsSocket.close()
    }
}

let _MZBenchWS = new MZBenchWS();

export default _MZBenchWS;

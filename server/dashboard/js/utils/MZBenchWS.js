class MZBenchWS {
    constructor() {
        this.wsSocket = undefined;
        this.notify = undefined;
    }

    connect(url, callbacks) {
        let {onopen, onmessage, onclose} = callbacks;

        let location = window.location;
        let protocol = "https:" == location.protocol ? "wss:" : "ws:";
        let wsUrl = protocol + "//" + location.host + url;

        let socket = new WebSocket(wsUrl);

        socket.onopen = (event) => {
            if (onopen) {
                onopen(socket);
            }
        }

        socket.onclose = (event) => {
            this.wsSocket = undefined;
            if (onclose) { onclose("disconnected"); }
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
        this.callbacks = callbacks;
        this.wsSocket = socket;
    }

    send(data) {
        if (this.isConnected()) {
            this.wsSocket.send(JSON.stringify(data));
        } else {
            console.log("WebSocket is not connected");
        }
    }

    isConnected() {
        return this.wsSocket && (this.wsSocket.readyState == WebSocket.OPEN);
    }

    close() {
        if (this.wsSocket) {
            this.wsSocket.onclose = (event) => {
                this.wsSocket = undefined;
                let {onopen, onmessage, onclose} = this.callbacks;
                if (onclose) { onclose("normal"); }
            };
            this.wsSocket.close()
        }
    }
}

let _MZBenchWS = new MZBenchWS();

export default _MZBenchWS;

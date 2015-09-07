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
            if (this.notify) {
                this.notify.update({message: 'The board has connected to the server', type: 'success'});
                setTimeout(() => {
                        this.notify.close();
                        this.notify = undefined;
                    }, 5000);
            }
            if (onopen) {
                onopen(socket);
            }
        }

        socket.onclose = (event) => {
            this.wsSocket = undefined;
            if (onclose) { onclose(); }
            setTimeout(() => this.connect(url, callbacks), 10000);
            if (!this.notify) {
                this.notify = $.notify(
                                { message: "The board is not connected to the server" },
                                { type: "danger", delay: 0 }
                              );
            }
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
        this.wsSocket.close()
    }
}

let _MZBenchWS = new MZBenchWS();

export default _MZBenchWS;

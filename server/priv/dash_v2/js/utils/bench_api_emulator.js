import ActionTypes from '../constants/ActionTypes';
import moment from 'moment';

// enable or disable emulator

let active = true;

function randInt(n) {
    return Math.floor((Math.random() * n));
}

function any(xs) {
    return xs[randInt(xs.length)];
};

function randStatus() {
    return any(["complete", "failed", "stopped"]);
}

function randScenario() {
    return any(["publish_subscribe.erl", "mosaic.erl"]);
}

function createFinishedBench(id) {
    let startTime = moment().add(-1 * randInt(20000), 'minutes');
    let finishTime = moment(startTime).add(randInt(600), 'seconds');

    return {
        id: id,
        startTime: startTime.format(),
        finishTime: finishTime.format(),
        metrics: dummyMetrics(id),
        demoGroups: dummyGroups(),
        graphite: "http://graphite.virt.aws.rtapi.net",
        scenario: randScenario(),
        status: randStatus(),
        scenarioBody: "[\n  %% Subscriber Process Pool  \n  {make_install, [{git, \"git@github.aws.rtapi.net:platform-software/pubsub\"},\n                  {branch, {var, \"branch\", \"master\"}},\n                  {dir, \"benchmarks/pubsub_worker\"}]},\n  {pool, [{size, {var, \"subscribers\", 1}},\n          %% Start workers at the rate of \n          {worker_start, {linear, {5000, rps}}},\n          %% Use pubsub worker\n          {worker_type, pubsub_worker}], [\n    %% Connect to the host and port using Websocket. Protocol can also be tcp\n    {connect, {var, \"protocol\", \"ws\"}, \"subscribe\", {var, \"rteng_host\", \"\"}, {var, \"rteng_port\", 8080}},\n\n    %% Set the channel name for the process. It concatenates the name of the channel with an integer which is picked from a sequence of integers. If there is more than one process in the pool, the round-robin function will pick a number from the sequence and assign it to the process. If there are more processes than channels, the processes can divided between the channel\n    {set_channel_name, {var, \"chan_pre\", \"channel\"}, {round_robin, {seq, 1, {var, \"channels\", 1}}}},\n    %% subscribe to the channel described above\n    {subscribe},\n    %% Set signal for publisher process to wait for all subscribers to start\n    {set_signal, wait_for_sub, 1},\n    %% Setup a receiver for the subscribed message. By default a maximum of 10 susbscribers will measure latency to reduce cpu cost\n    %% on the bench\n    {msg_receiver, [{t, timeout, 60000},\n                    {t, buffer, 0},\n                    {t, reporting_subs, {var, \"reporting_subs\", 10}}]}\n\n  ]},\n\n  %% Publisher pool. Used to publish the message\n  {pool, [{size, {var, \"publishers\", 1}},\n          {worker_type, pubsub_worker}], [\n\n    %% connect to rtm using Websocket\n    {connect, {var, \"protocol\", \"ws\"}, \"publish\",  {var, \"rteng_host\", \"\"}, {var, \"rteng_port\", 8080}},\n\n    %% Set channel name\n    {set_channel_name, {var, \"chan_pre\", \"channel\"}, {round_robin, {seq, 1, {var, \"channels\", 1}}}},\n\n    %% Wait for subscribers to start. The number of signals should be equal to the number of subscriber processes\n    {wait_signal, wait_for_sub, {var, \"subscribers\", 1}},\n\n    %% Spawn acknowledgement reciever to receive the publish and acknowledgement and measure publish latency\n    {spawn_acker, 60000},\n    %% Ramp up loop to publish messages \n    {loop, [ {time, {{var, \"ramp_time\", 5}, min}},\n             %% Start RPS and End RPS for Ramp\n             {rate,  {ramp, linear, {{var , \"start_rps\", 1}, rps},\n                     {{var , \"end_rps\", 1}, rps}}},\n                     {iterator, \"req_rate\"}],\n     [\n      %% Publish the message with batch size and message size. For a high publishing throughput > 1000 RPS, use batching to improve\n      %% performance. \n        {publish, {var, \"batch\", 1}, {numvar, \"message_size\", 100}}\n     ]},\n    %% After ramp, run steady for N mins \n    {loop, [ {time, {{var , \"steady_time\", 5}, min}},\n             {rate,  {{var, \"end_rps\", 1}, rps}} ],\n     [\n      {publish, {var, \"batch\", 1}, {numvar, \"message_size\", 100}}\n     ]},\n    {disconnect}\n  ]}\n].\n",
    };
}

function runBenchPipeline(bench, onData, statuses) {
    if (!active) return;

    let [status, ...rest] = statuses;

    bench.status = status;

    if (0 == rest.length) {
        bench.finishTime = moment().format();
    };

    if (status == "provisioning") {
        bench.graphite = "http://graphite.virt.aws.rtapi.net";
        bench.demoGroups = dummyGroups();
        bench.metrics = dummyMetrics(bench.id);
    }

    onData({type: ActionTypes.UPDATE_BENCH_INFO, data: bench});

    if (0 != rest.length) {
        setTimeout(() => runBenchPipeline(bench, onData, rest), 20000)
    }
}

function emulateBenchActivity(idx, onData) {
    if (!active) return;

    let status = randStatus();
    let statuses = ["initializing", "provisioning", "running", status];

    let bench = {
        id: idx,
        startTime: moment().format(),
        scenario: randScenario(),
        scenarioBody: "[\n  %% Subscriber Process Pool  \n  {make_install, [{git, \"git@github.aws.rtapi.net:platform-software/pubsub\"},\n                  {branch, {var, \"branch\", \"master\"}},\n                  {dir, \"benchmarks/pubsub_worker\"}]},\n  {pool, [{size, {var, \"subscribers\", 1}},\n          %% Start workers at the rate of \n          {worker_start, {linear, {5000, rps}}},\n          %% Use pubsub worker\n          {worker_type, pubsub_worker}], [\n    %% Connect to the host and port using Websocket. Protocol can also be tcp\n    {connect, {var, \"protocol\", \"ws\"}, \"subscribe\", {var, \"rteng_host\", \"\"}, {var, \"rteng_port\", 8080}},\n\n    %% Set the channel name for the process. It concatenates the name of the channel with an integer which is picked from a sequence of integers. If there is more than one process in the pool, the round-robin function will pick a number from the sequence and assign it to the process. If there are more processes than channels, the processes can divided between the channel\n    {set_channel_name, {var, \"chan_pre\", \"channel\"}, {round_robin, {seq, 1, {var, \"channels\", 1}}}},\n    %% subscribe to the channel described above\n    {subscribe},\n    %% Set signal for publisher process to wait for all subscribers to start\n    {set_signal, wait_for_sub, 1},\n    %% Setup a receiver for the subscribed message. By default a maximum of 10 susbscribers will measure latency to reduce cpu cost\n    %% on the bench\n    {msg_receiver, [{t, timeout, 60000},\n                    {t, buffer, 0},\n                    {t, reporting_subs, {var, \"reporting_subs\", 10}}]}\n\n  ]},\n\n  %% Publisher pool. Used to publish the message\n  {pool, [{size, {var, \"publishers\", 1}},\n          {worker_type, pubsub_worker}], [\n\n    %% connect to rtm using Websocket\n    {connect, {var, \"protocol\", \"ws\"}, \"publish\",  {var, \"rteng_host\", \"\"}, {var, \"rteng_port\", 8080}},\n\n    %% Set channel name\n    {set_channel_name, {var, \"chan_pre\", \"channel\"}, {round_robin, {seq, 1, {var, \"channels\", 1}}}},\n\n    %% Wait for subscribers to start. The number of signals should be equal to the number of subscriber processes\n    {wait_signal, wait_for_sub, {var, \"subscribers\", 1}},\n\n    %% Spawn acknowledgement reciever to receive the publish and acknowledgement and measure publish latency\n    {spawn_acker, 60000},\n    %% Ramp up loop to publish messages \n    {loop, [ {time, {{var, \"ramp_time\", 5}, min}},\n             %% Start RPS and End RPS for Ramp\n             {rate,  {ramp, linear, {{var , \"start_rps\", 1}, rps},\n                     {{var , \"end_rps\", 1}, rps}}},\n                     {iterator, \"req_rate\"}],\n     [\n      %% Publish the message with batch size and message size. For a high publishing throughput > 1000 RPS, use batching to improve\n      %% performance. \n        {publish, {var, \"batch\", 1}, {numvar, \"message_size\", 100}}\n     ]},\n    %% After ramp, run steady for N mins \n    {loop, [ {time, {{var , \"steady_time\", 5}, min}},\n             {rate,  {{var, \"end_rps\", 1}, rps}} ],\n     [\n      {publish, {var, \"batch\", 1}, {numvar, \"message_size\", 100}}\n     ]},\n    {disconnect}\n  ]}\n].\n",
    };

    setTimeout(() => emulateBenchActivity(idx+1, onData), 90000);
    runBenchPipeline(bench, onData, statuses);
}

function dummyGroups() {
    var groups = [
        {
          name: "Publishers",
          graphs: [
            {
              name:    "Publisher count (abs)",
              metrics: ["publish.count.value"]
            },
            {
              name:    "Publisher count (rps)",
              metrics: ["publish.count.rps.value"]
            },
            {
              name:    "Publisher latencies",
              metrics: ["publish.latency.min.value",
                        "publish.latency.max.value",
                        "publish.latency.mean.value",
                        "publish.latency.50.value",
                        "publish.latency.75.value",
                        "publish.latency.90.value",
                        "publish.latency.95.value",
                        "publish.latency.99.value",
                        "publish.latency.999.value"],
            }
          ]
        },
        {
          name: "Subscribers",
          graphs: [
            {
              name:    "Subscribers count (abs)",
              metrics: ["subscribe.count.value"]
            },
            {
              name:    "Subscriber count (rps)",
              metrics: ["subscribe.count.rps.value"]
            },
            {
              name:    "Subscriber latencies",
              metrics: ["subscribe.latency.min.value",
                        "subscribe.latency.max.value",
                        "subscribe.latency.mean.value",
                        "subscribe.latency.50.value",
                        "subscribe.latency.75.value",
                        "subscribe.latency.90.value",
                        "subscribe.latency.95.value",
                        "subscribe.latency.99.value",
                        "subscribe.latency.999.value"],
            }
          ]
        },
        {
          name: "Data transmission",
          graphs: [
            {
              name:    "Sample Sub Messages received (bytes)",
              metrics: ["subscribe.receive_message.size.rps.value"],
            },
            {
              name:    "Total Data Size received (rps)",
              metrics: ["publish.data_size.rps.value",
                        "subscribe.data_size.rps.value"]
            }
          ]
        },
        {
          name: "Connections",
          graphs: [
            {
              name:    "Lost connections (abs)",
              metrics: ["publish.receive_ack.failed.value",
                        "publish.receive_ack.connection_closed.value",
                        "subscribe.receive_message.failed.value",
                        "subscribe.receive_message.connection_closed.value",
                        "publish.send.failed.value",
                        "publish.send.connection_closed.value",
                        "reconnect.value"]
            },
            {
              name:    "Lost connections (rps)",
              metrics: ["publish.receive_ack.failed.rps.value",
                        "publish.receive_ack.connection_closed.rps.value",
                        "subscribe.receive_message.failed.rps.value",
                        "subscribe.receive_message.connection_closed.rps.value",
                        "publish.send.failed.rps.value",
                        "publish.send.connection_closed.rps.value",
                        "reconnect.rps.value"]
            }
          ]
        },
        {
          name: "System metrics",
          graphs: [
            {
              name:    "Load average",
              metrics: ["systemload.la1.mzb_director1209_b1.value",
                        "systemload.la1.mzb_worker1209_b0.value"],
            },
            {
              name:    "CPU",
              metrics: ["systemload.cpu.mzb_director1209_b1.value",
                        "systemload.cpu.mzb_worker1209_b0.value"],
            },
            {
              name:    "RAM",
              metrics: ["systemload.ram.mzb_director1209_b1.value",
                        "systemload.ram.mzb_worker1209_b0.value"],
            },
          ]
        },
    ];

    return groups.map((group) => {
        group.graphs = group.graphs.map((graphs) => {
            graphs.metrics = graphs.metrics.map((name) => {
                return `publish_subscribe.mzb.${name}`
            });
            return graphs;
        });
        return group;
    });
}

function dummyMetrics(benchId) {
    var metrics = [["publish.count.value"],
                   ["publish.count.rps.value"],
                   ["subscribe.count.value"],
                   ["subscribe.count.rps.value"],
                   ["subscribe.receive_message.size.value"],
                   ["subscribe.receive_message.size.rps.value"],
                   ["publish.latency.min.value",
                    "publish.latency.max.value",
                    "publish.latency.mean.value",
                    "publish.latency.50.value",
                    "publish.latency.75.value",
                    "publish.latency.90.value",
                    "publish.latency.95.value",
                    "publish.latency.99.value",
                    "publish.latency.999.value"],
                   ["subscribe.latency.max.value",
                    "subscribe.latency.mean.value",
                    "subscribe.latency.50.value",
                    "subscribe.latency.75.value",
                    "subscribe.latency.90.value",
                    "subscribe.latency.95.value",
                    "subscribe.latency.99.value",
                    "subscribe.latency.999.value"],
                   ["publish.data_size.rps.value",
                    "subscribe.data_size.rps.value"],
                   ["publish.receive_ack.failed.value",
                    "publish.receive_ack.connection_closed.value",
                    "subscribe.receive_message.failed.value",
                    "subscribe.receive_message.connection_closed.value",
                    "publish.send.failed.value",
                    "publish.send.connection_closed.value",
                    "reconnect.value"],
                   ["publish.receive_ack.failed.rps.value",
                    "publish.receive_ack.connection_closed.rps.value",
                    "subscribe.receive_message.failed.rps.value",
                    "subscribe.receive_message.connection_closed.rps.value",
                    "publish.send.failed.rps.value",
                    "publish.send.connection_closed.rps.value",
                    "reconnect.rps.value"],
                   ["systemload.la1.mzb_director1209_b1.value",
                    "systemload.la1.mzb_worker1209_b0.value"],
                   ["systemload.cpu.mzb_director1209_b1.value",
                    "systemload.cpu.mzb_worker1209_b0.value"],
                   ["systemload.ram.mzb_director1209_b1.value",
                    "systemload.ram.mzb_worker1209_b0.value"],
                   ["systemload.nettx.mzb_director1209_b1.value",
                    "systemload.nettx.mzb_worker1209_b0.value"],
                   ["systemload.netrx.mzb_director1209_b1.value",
                    "systemload.netrx.mzb_worker1209_b0.value"],
                   ["systemload.interval.mzb_director1209_b1.value",
                    "systemload.interval.mzb_worker1209_b0.value"],
                   ["metric_merging_time.value"]];

    return metrics.map((group) => group.map((name) => `mzd.${benchId}.${name}`));
}


function emulateMetricReporter(benchId, onData) {
    let metrics = dummyMetrics(benchId);
    metrics.forEach((group) => group.forEach((metric) => {
        onData({type: ActionTypes.METRIC_UPDATE, name: metric, value: randInt(200)});
    }))
}

export function subscribeBenchTimeline({params, onData, onClose}) {
    let { initDataLimit = 20 } = params;
    active = true;
    let idx = 0;
    let benches = [];
    for (; idx < initDataLimit; ++idx) {
        let bench = createFinishedBench(idx);
        benches.push(bench);
    }
    console.log(JSON.stringify(benches).length);
    onData({type: ActionTypes.BENCH_INIT_DATA, data: benches, autoUpdate: true});
    setTimeout(() => emulateBenchActivity(idx, onData), 10000);
}

export function unsubscribeBenchTimeline(config) {
    active = false;
}

export function subscribeMetrics({params, onData, onClose}) {
    let { benchId } = params;
    let interval=  setInterval(() => emulateMetricReporter(benchId, onData), 2000);
    console.log("SET INTERVAL " + interval);
    return interval;
}

export function unsubscribeMetrics(interval) {
    console.log("unsubscribeMetrics " + interval);
    clearInterval(interval);
}

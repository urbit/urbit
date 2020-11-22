window.urb = new Channel();

const sendSubData = (msg) => {
    window.urb.poke(window.ship, 'chanel', 'chanel-action',
                    {'send-sub-data': {'path': '/example', 'msg': msg}},
                    () => 'sent', (err) => console.log(err));
};

const doShipCalls = () => {
    console.log(`window.ship: ${window.ship}`);

    window.urb.poke(window.ship, 'chanel', 'chanel-action', {'increase-counter': {step: 40}}, () => console.log("Successful poke"), (err) => console.log(err));
    window.urb.poke(window.ship, 'chanel', 'chanel-action', {example: {who: 'timluc-miptev', msg: 'hello world', app: 'chanel', friends: ['zod', 'dopzod', 'timluc']}}, () => console.log("Successful poke"), (err) => console.log(err));
    window.urb.poke(window.ship, 'chanel', 'json', {'key1': 9}, () => console.log("JSON poke"), (err) => console.log(err));

    // subscriptions
    window.urb.subscribe(window.ship, 'chanel', '/example', (err) => console.log("Sub Error"), (data) => console.log(`got response: ${data}`), () => console.log("Sub Quit"));

};

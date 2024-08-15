let pith;
let ship;
let eventSource;
let activeSubIds= {};
let channelMessageId = 0;
const channelId = `${Date.now()}${Math.floor(Math.random() * 100)}`;
const channelPath = `${window.location.origin}/~/channel/${channelId}`;
const baseSubPath = '/eyre-chan/mast';
const gallApp = 'neo';
addEventListener('DOMContentLoaded', async () => {
    pith = document.documentElement.getAttribute('pith');
    ship = document.documentElement.getAttribute('ship');
    let subKey = document.body.getAttribute('key');
    if (!subKey) subKey = document.body.firstElementChild.getAttribute('key');
    await connectToShip(subKey);
    let eventElements = document.querySelectorAll('[event]');
    eventElements.forEach(el => setEventListeners(el));
    handleKidElements([...document.querySelectorAll('[kid]')]);
    window.addEventListener('beforeunload', () => {
        const subKeys = [...document.querySelectorAll('[kid]')].map(el => {
            el.getAttribute('key');
        });
        subKeys.push(subKey);
        closeSubscriptions(subKeys);
    });
});
async function connectToShip(subKey) {
    await fetch(channelPath, { 
        method: 'PUT',
        body: JSON.stringify(makeSubscribeBody(subKey))
    });
    eventSource = new EventSource(channelPath);
    eventSource.addEventListener('message', handleChannelStream);
};
function setEventListeners(el) {
    const eventAttrVals = el.getAttribute('event');
    const returnAttrVals = el.getAttribute('return');
    const throttleMs = Number(el.getAttribute('throttle')) * 1000;
    const debounceMs = Number(el.getAttribute('debounce')) * 1000;
    eventAttrVals.split(/\s+/).forEach(eventAttr => {
        let splitEventAttr = eventAttr.split('/');
        if (splitEventAttr[0] === '') splitEventAttr.shift();
        const eventType = splitEventAttr[0];
        if (throttleMs) {
            el[`on${eventType}`] = pokeThrottle(throttleMs, eventType, eventAttr, returnAttrVals);
        } else if (debounceMs) {
            el[`on${eventType}`] = pokeDebounce(debounceMs, eventType, eventAttr, returnAttrVals);
        } else {
            el[`on${eventType}`] = (e) => pokeShip(e, e.currentTarget, eventType, eventAttr, returnAttrVals);
        };
    });
};
function pokeThrottle(ms, ...pokeArgs) {
    let ready = true;
    return (e) => {
        if (!ready) return;
        ready = false;
        window.setTimeout(() => { ready = true; }, ms);
        pokeShip(e, e.currentTarget, ...pokeArgs);
    };
};
function pokeDebounce(ms, ...pokeArgs) {
    let timeoutId = null;
    return (e) => {
        window.clearTimeout(timeoutId);
        timeoutId = window.setTimeout(() => pokeShip(e, e.target, ...pokeArgs), ms);
    };
};
function pokeShip(event, target, eventType, eventAttr, returnAttrVals) {
    let parentComponent = target.closest('[rope]');
    const rope = Number(parentComponent.getAttribute('rope'));
    const jsOnEvent = target.getAttribute('js-on-event');
    if (jsOnEvent) {
        eval?.(`"use strict"; ${jsOnEvent}`);
    };
    let uiEventData = {};
    if (returnAttrVals) {
        uiEventData = handleReturnAttr(event, target, returnAttrVals);
    };
    if (eventType === 'submit') {
        event.preventDefault();
        const formData = new FormData(target);
        formData.forEach((v, k) => { uiEventData[k] = v });
        target.reset();
    };
    fetch(channelPath, {
        method: 'PUT',
        body: JSON.stringify(makePokeBody({
            rope,
            path: eventAttr,
            data: uiEventData
        }))
    });
};
function handleReturnAttr(event, target, returnAttrVals) {
    let returnData = {};
    returnAttrVals.split(/\s+/).forEach(returnAttr => {
        let splitReturnAttr = returnAttr.split('/');
        if (splitReturnAttr[0] === '') splitReturnAttr.shift();
        const returnObjSelector = splitReturnAttr[0];
        const key = splitReturnAttr[1];
        if (returnObjSelector === 'event') {
            if (!(key in event)) {
                console.error(`Property: ${key} does not exist on the event object`);
                return;
            };
            returnData[returnAttr] = String(event[key]);
        } else {
            let returnObj;
            if (returnObjSelector === 'target') {
                returnObj = target;
            } else {
                const linkedEl = document.getElementById(returnObjSelector);
                if (!linkedEl) {
                    console.error(`No element found for id: ${returnObjSelector}`);
                    return;
                };
                returnObj = linkedEl;
            };
            if (key.startsWith('data')) {
                const dataKey = key.substring(5).split('-').map((w, i) => {
                    if (i === 0) {
                        return w.toLowerCase();
                    } else {
                        return w.charAt(0).toUpperCase() + w.slice(1).toLowerCase();
                    };
                }).join('');
                if (!returnObj.dataset.hasOwnProperty(dataKey)) {
                    console.error(`Property: ${dataKey} does not exist on the specified object`);
                    return;
                };
                returnData[returnAttr] = String(returnObj.dataset[dataKey]);
            } else {
                if (!(key in returnObj)) {
                    console.error(`Property: ${key} does not exist on the specified object`);
                    return;
                };
                // TODO: handle other properties that don't cast to string
                returnData[returnAttr] = String(returnObj[key]);
            };
        };
    });
    return returnData;
};
function handleChannelStream(event) {
    const streamResponse = JSON.parse(event.data);
    // console.log(streamResponse);
    if (streamResponse.response !== 'diff') return;
    fetch(channelPath, {
        method: 'PUT',
        body: JSON.stringify(makeAck(streamResponse.id))
    });
    if (!Object.values(activeSubIds).includes(streamResponse.id)) return;
    streamResponse.json.forEach(gustObj => {
        switch (gustObj.p) {
            case 'd':
                gustObj.q.forEach(key => {
                    let toRemove = document.querySelector(`[key="${key}"]`);
                    const jsOnDelete = toRemove.getAttribute('js-on-delete');
                    if (jsOnDelete) {
                        eval?.(`"use strict"; ${jsOnDelete}`);
                    };
                    let kidEls = [...toRemove.querySelectorAll('[kid]')];
                    if (toRemove.hasAttribute('kid')) kidEls.push(toRemove);
                    if (kidEls.length > 0) {
                        const subKeys = kidEls.map(el => el.getAttribute('key'));
                        closeSubscriptions(subKeys);
                    };
                    toRemove.remove();
                });
                break;
            case 'n':
                let parent = document.querySelector(`[key="${gustObj.q}"]`);
                if (gustObj.r === 0) {
                    parent.insertAdjacentHTML('afterbegin', gustObj.s);
                } else if (gustObj.r === parent.childNodes.length) {
                    parent.insertAdjacentHTML('beforeend', gustObj.s);
                } else {
                    let indexTarget = parent.childNodes[gustObj.r];
                    if (indexTarget.nodeType === 1) {
                        indexTarget.insertAdjacentHTML('beforebegin', gustObj.s);
                    } else {
                        let placeholder = document.createElement('div');
                        parent.insertBefore(placeholder, indexTarget);
                        placeholder = parent.childNodes[gustObj.r];
                        placeholder.outerHTML = gustObj.s;
                    };
                };
                let newNode = parent.childNodes[gustObj.r];
                if (newNode.nodeType === 1) {
                    if (newNode.getAttribute('event')) {
                        setEventListeners(newNode);
                    };
                    if (newNode.childElementCount > 0) {
                        let needingListeners = newNode.querySelectorAll('[event]');
                        needingListeners.forEach(el => setEventListeners(el));
                    };
                    if (newNode.nodeName === 'KID') {
                        handleKidElements([newNode]);
                    } else {
                        handleKidElements([...newNode.getElementsByTagName('kid')]);
                    };
                    const jsOnAdd = newNode.getAttribute('js-on-add');
                    if (jsOnAdd) {
                        eval?.(`"use strict"; ${jsOnAdd}`);
                    };
                };
                break;
            case 'm':
                let fromNode = document.querySelector(`[key="${gustObj.q}"]`);
                const fromIndex = [ ...fromNode.parentNode.childNodes ].indexOf(fromNode);
                if (fromIndex < gustObj.r) gustObj.r++;
                let toNode = fromNode.parentNode.childNodes[gustObj.r];
                fromNode.parentNode.insertBefore(fromNode, toNode);
                break;
            case 'c':
                let targetNode = document.querySelector(`[key="${gustObj.q}"]`);
                if (gustObj.r.length) {
                    gustObj.r.forEach(attr => {
                        if (attr === 'event') {
                            let eventVal = targetNode.getAttribute('event').split('/');
                            if (eventVal[0] === '') eventVal.shift();
                            const eventType = eventVal[0];
                            targetNode[`on${eventType}`] = null;
                        };
                        targetNode.removeAttribute(attr);
                    });
                };
                if (gustObj.s.length) {
                    gustObj.s.forEach(attr => {
                        const name = attr[0];
                        const value = attr[1];
                        targetNode.setAttribute(name, value);
                        if (name === 'event') setEventListeners(targetNode);
                    });
                };
                break;
            case 't':
                let textWrapperNode = document.querySelector(`[key="${gustObj.q}"]`);
                textWrapperNode.textContent = gustObj.r;
                break;
            case 'k':
                let kidPlaceholder = document.querySelector(`[key="${gustObj.q}"]`);
                kidPlaceholder.outerHTML = gustObj.r;
                let kid = document.querySelector(`[key="${gustObj.q}"]`);
                if (kid.hasAttribute('event')) {
                    setEventListeners(kid);
                };
                if (kid.childElementCount > 0) {
                    let needingListeners = kid.querySelectorAll('[event]');
                    needingListeners.forEach(el => setEventListeners(el));
                };
                handleKidElements([...kid.getElementsByTagName('kid')]);
                break;
        };
    });
};
function handleKidElements(kidElements) {
    kidElements.forEach(el => {
        let key = el.getAttribute('key');
        fetch(channelPath, { 
            method: 'PUT',
            body: JSON.stringify(makeSubscribeBody(key))
        });
    });
};
function closeSubscriptions(keyArray) {
    const actionArray = keyArray.map(key => {
        channelMessageId++;
        const subMsgId = activeSubIds[key];
        delete activeSubIds[key];
        return {
            id: channelMessageId,
            action: 'unsubscribe',
            subscription: subMsgId
        };
    });
    fetch(channelPath, {
        method: 'PUT',
        body: JSON.stringify(actionArray)
    });
};
function makeSubscribeBody(subKey) {
    channelMessageId++;
    activeSubIds[subKey] = channelMessageId;
    return [{
        id: channelMessageId,
        action: 'subscribe',
        ship: ship,
        app: gallApp,
        path: `${baseSubPath}/${subKey}`
    }];
};
function makePokeBody(jsonData) {
    channelMessageId++;
    return [{
        id: channelMessageId,
        action: 'poke',
        ship: ship,
        app: gallApp,
        mark: 'json',
        json: { pith: pith, data: jsonData }
    }];
};
function makeAck(eventId) {
    channelMessageId++;
    return [{
        id: channelMessageId,
        action: 'ack',
        "event-id": eventId
    }];
};

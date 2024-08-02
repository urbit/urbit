let rope;
let pith;
let path;
let ship;
let app;
let channelMessageId = 0;
let eventSource;
const channelId = `${Date.now()}${Math.floor(Math.random() * 100)}`;
const channelPath = `${window.location.origin}/~/channel/${channelId}`;
addEventListener('DOMContentLoaded', async () => {
    rope = Number(document.documentElement.getAttribute('rope'));
    pith = document.documentElement.getAttribute('pith');
    path = document.documentElement.getAttribute('path');
    ship = document.documentElement.getAttribute('ship');
    app = document.documentElement.getAttribute('app');
    await connectToShip();
    let eventElements = document.querySelectorAll('[event]');
    eventElements.forEach(el => setEventListeners(el));
});
async function connectToShip() {
    try {
        const storageKey = `${ship}${app}${path}`;
        let storedId = localStorage.getItem(storageKey);
        localStorage.setItem(storageKey, channelId);
        if (storedId) {
            const delPath = `${window.location.origin}/~/channel/${storedId}`;
            await fetch(delPath, {
                method: 'PUT',
                body: JSON.stringify([{
                    id: channelMessageId,
                    action: 'delete'
                }])
            });
        };
        const body = JSON.stringify(makeSubscribeBody());
        await fetch(channelPath, { 
            method: 'PUT',
            body
        });
        eventSource = new EventSource(channelPath);
        eventSource.addEventListener('message', handleChannelStream);
    } catch (error) {
        console.error(error);
    };
};
function setEventListeners(el) {
    const eventAttrVals = el.getAttribute('event');
    const returnAttrVals = el.getAttribute('return');
    eventAttrVals.split(/\s+/).forEach(eventAttr => {
        let splitEventAttr = eventAttr.split('/');
        if (splitEventAttr[0] === '') splitEventAttr.shift();
        const eventType = splitEventAttr[0];
        el[`on${eventType}`] = (e) => pokeShip(e, eventType, eventAttr, returnAttrVals);
    });
};
function pokeShip(event, eventType, eventAttr, returnAttrVals) {
    let uiEventData = {};
    if (returnAttrVals) {
        uiEventData = handleReturnAttr(event, returnAttrVals);
    };
    if (eventType === 'submit') {
        event.preventDefault();
        const formData = new FormData(event.target);
        formData.forEach((v, k) => { uiEventData[k] = v });
        event.target.reset();
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
function handleReturnAttr(event, returnAttrVals) {
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
                returnObj = event.currentTarget;
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
    try {
        const streamResponse = JSON.parse(event.data);
        if (streamResponse.response !== 'diff') return;
        fetch(channelPath, {
            method: 'PUT',
            body: JSON.stringify(makeAck(streamResponse.id))
        });
        const htmlData = streamResponse.json;
        // console.log(htmlData);
        if (!htmlData) return;
        let container = document.createElement('template');
        container.innerHTML = htmlData;
        if (container.content.firstElementChild.childNodes.length === 0) return;
        // const navUrl = container.content.firstElementChild.getAttribute('url');
        // if (navUrl && (navUrl !== window.location.pathname)) {
        //     history.pushState({}, '', navUrl);
        // };
        while (container.content.firstElementChild.children.length > 0) {
            let gustChild = container.content.firstElementChild.firstElementChild;
            if (gustChild.tagName === 'D') {
                for (const att of gustChild.attributes) {
                    const dkey = att.value;
                    document.querySelector(`[key="${dkey}"]`).remove();
                };
                gustChild.remove();
            } else if (gustChild.tagName === 'N') {
                const nodeKey = gustChild.firstElementChild.getAttribute('key');
                const parentKey = gustChild.firstElementChild.getAttribute('pkey');
                const appendIndex = gustChild.id;
                let domParent = document.querySelector(`[key="${parentKey}"]`);
                domParent.insertBefore(gustChild.firstElementChild, domParent.children[appendIndex]);
                let appendedChild = domParent.querySelector(`[key="${nodeKey}"]`);
                if (appendedChild.getAttribute('event')) {
                    setEventListeners(appendedChild);
                };
                if (appendedChild.childElementCount > 0) {
                    let needingListeners = appendedChild.querySelectorAll('[event]');
                    needingListeners.forEach(child => setEventListeners(child));
                };
                appendedChild = appendedChild.nextElementSibling;
                gustChild.remove();
            } else if (gustChild.tagName === 'M') {
                const nodeKey = gustChild.getAttribute('key');
                const nodeIndex = gustChild.id;
                let existentNode = document.querySelector(`[key="${nodeKey}"]`);
                let childAtIndex = existentNode.parentElement.children[nodeIndex];
                if (existentNode.nextElementSibling 
                && (existentNode.nextElementSibling.getAttribute('key') 
                === childAtIndex.getAttribute('key'))) {
                    existentNode.parentElement.insertBefore(existentNode, childAtIndex.nextElementSibling);
                } else {
                    existentNode.parentElement.insertBefore(existentNode, childAtIndex);
                };
                gustChild.remove();
            } else if (gustChild.tagName === 'C') {
                const nodeKey = gustChild.getAttribute('key');
                const attToRem = gustChild.getAttribute('rem')?.slice(0, -1).split(' ') ?? [];
                let existentNode = document.querySelector(`[key="${nodeKey}"]`);
                attToRem.forEach(att => {
                    if (att === 'event') {
                        const eventType = existentNode.getAttribute('event').split('/', 2)[1];
                        existentNode[`on${eventType}`] = null;
                    };
                    existentNode.removeAttribute(att);
                });
                gustChild.removeAttribute('key');
                gustChild.removeAttribute('rem');
                for (const att of gustChild.attributes) {
                    existentNode.setAttribute(att.name, att.value);
                    if (att.name === 'event') {
                        const eventType = existentNode.getAttribute('event').split('/', 2)[1];
                        existentNode[`on${eventType}`] = null;
                        setEventListeners(existentNode);
                    };
                };
                gustChild.remove();
            } else {
                const nodeKey = gustChild.getAttribute('key');
                let existentNode = document.querySelector(`[key="${nodeKey}"]`);
                existentNode.replaceWith(gustChild);
                let replacedNode = document.querySelector(`[key="${nodeKey}"]`);
                if (replacedNode.getAttribute('event')) {
                    setEventListeners(replacedNode);
                };
                if (replacedNode.childElementCount > 0) {
                    let needingListeners = replacedNode.querySelectorAll('[event]');
                    needingListeners.forEach(child => setEventListeners(child));
                };
            };
        };
    } catch (error) {
        console.error(error);
    };
};
function makeSubscribeBody() {
    channelMessageId++;
    return [{
        id: channelMessageId,
        action: 'subscribe',
        ship: ship,
        app: app,
        path: path
    }];
};
function makePokeBody(jsonData) {
    channelMessageId++;
    return [{
        id: channelMessageId,
        action: 'poke',
        ship: ship,
        app: app,
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

let pith;
let path;
let ship;
let app;
let channelMessageId = 0;
let eventSource;
const channelId = `${Date.now()}${Math.floor(Math.random() * 100)}`;
const channelPath = `${window.location.origin}/~/channel/${channelId}`;
addEventListener('DOMContentLoaded', async () => {
    pith = document.documentElement.getAttribute('pith');
    path = document.documentElement.getAttribute('path');
    ship = document.documentElement.getAttribute('ship');
    app = document.documentElement.getAttribute('app');
    await connectToShip();
    let eventElements = document.querySelectorAll('[event]');
    eventElements.forEach(el => setEventListeners(el));
});
function setEventListeners(el) {
    const eventTags = el.getAttribute('event');
    const returnTags = el.getAttribute('return');
    eventTags.split(/\s+/).forEach(eventStr => {
        const eventType = eventStr.split('/', 2)[1];
        el[`on${eventType}`] = (e) => pokeShip(e, eventStr, returnTags);
    });
};
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
function pokeShip(event, tagString, dataString) {
    try {
        let data = {};
        if (dataString) {
            const dataToReturn = dataString.split(/\s+/);
            dataToReturn.forEach(dataTag => {
                let splitDataTag = dataTag.split('/');
                if (splitDataTag[0] === '') splitDataTag.shift();
                const kind = splitDataTag[0];
                const key = splitDataTag.pop();
                if (kind === 'event') {
                    if (!(key in event)) {
                        console.error(`Property: ${key} does not exist on the event object`);
                        return;
                    };
                    data[dataTag] = String(event[key]);
                } else if (kind === 'target') {
                    if (!(key in event.currentTarget)) {
                        console.error(`Property: ${key} does not exist on the target object`);
                        return;
                    };
                    data[dataTag] = String(event.currentTarget[key]);
                } else {
                    const elementId = splitDataTag.join('/');
                    const linkedEl = document.getElementById(elementId);
                    if (!linkedEl) {
                        console.error(`No element found for id: ${kind}`);
                        return;
                    };
                    if (!(key in linkedEl)) {
                        console.error(`Property: ${key} does not exist on the object with id: ${elementId}`);
                        return;
                    };
                    data[dataTag] = String(linkedEl[key]);
                };
            });
        };
        fetch(channelPath, {
            method: 'PUT',
            body: JSON.stringify(makePokeBody({
                path: tagString,
                data
            }))
        });
    } catch (error) {
        console.error(error);
    };
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

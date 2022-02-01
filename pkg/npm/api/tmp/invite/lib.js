export const inviteAction = (data) => ({
    app: 'invite-store',
    mark: 'invite-action',
    json: data
});
export const accept = (app, uid) => inviteAction({
    accept: {
        term: app,
        uid
    }
});
export const decline = (app, uid) => inviteAction({
    decline: {
        term: app,
        uid
    }
});
//# sourceMappingURL=lib.js.map
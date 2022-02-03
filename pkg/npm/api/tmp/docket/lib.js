export function chadIsRunning(chad) {
    return 'glob' in chad || 'site' in chad;
}
export const scryCharges = {
    app: 'docket',
    path: '/charges'
};
export const scryDockets = {
    app: 'docket',
    path: '/dockets'
};
export const scryTreaties = {
    app: 'treaty',
    path: '/treaties'
};
export const scryDefaultAlly = {
    app: 'treaty',
    path: '/default-ally'
};
export const scryAllies = {
    app: 'treaty',
    path: '/allies'
};
export const scryAllyTreaties = (ship) => ({
    app: 'treaty',
    path: `/treaties/${ship}`
});
/**
 * Uninstall a desk, and remove docket
 */
export function docketUninstall(desk) {
    return {
        app: 'docket',
        mark: 'docket-uninstall',
        json: desk
    };
}
export function docketInstall(ship, desk) {
    return {
        app: 'docket',
        mark: 'docket-install',
        json: `${ship}/${desk}`
    };
}
export function allyShip(ship) {
    return {
        app: 'treaty',
        mark: 'ally-update-0',
        json: {
            add: ship
        }
    };
}
//# sourceMappingURL=lib.js.map
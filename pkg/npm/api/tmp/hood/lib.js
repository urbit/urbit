export const getVats = {
    app: 'hood',
    path: '/kiln/vats'
};
/**
 * Install a foreign desk
 */
export function kilnInstall(ship, desk, local) {
    return {
        app: 'hood',
        mark: 'kiln-install',
        json: {
            ship,
            desk,
            local: local || desk
        }
    };
}
/**
 * Uninstall a desk
 */
export function kilnUninstall(desk) {
    return {
        app: 'hood',
        mark: 'kiln-uninstall',
        json: desk
    };
}
export function kilnSuspend(desk) {
    return {
        app: 'hood',
        mark: 'kiln-suspend',
        json: desk
    };
}
export function kilnRevive(desk) {
    return {
        app: 'hood',
        mark: 'kiln-revive',
        json: desk
    };
}
export function kilnBump(force = false, except = []) {
    return {
        app: 'hood',
        mark: 'kiln-bump',
        json: {
            force,
            except
        }
    };
}
export function kilnPause(desk) {
    return {
        app: 'hood',
        mark: 'kiln-pause',
        json: desk
    };
}
export function kilnResume(desk) {
    return {
        app: 'hood',
        mark: 'kiln-resume',
        json: desk
    };
}
export const scryLag = ({ app: 'hood', path: '/kiln/lag' });
export function getBlockers(vats) {
    const blockers = [];
    const base = vats?.base;
    if (!base) {
        return blockers;
    }
    const blockedOn = base.arak.rail?.next?.[0]?.weft?.kelvin;
    if (!blockedOn) {
        return blockers;
    }
    Object.entries(vats)
        .filter(([desk]) => desk !== 'base')
        .forEach(([desk, vat]) => {
        // assuming only %zuse
        const woofs = vat.arak.rail?.next || [];
        const kelvins = woofs.map(n => n.weft.kelvin);
        if (!(kelvins.includes(blockedOn))) {
            blockers.push(desk);
        }
    });
    return blockers;
}
export function getVatPublisher(vat) {
    if (vat.arak.rail) {
        const { rail } = vat.arak;
        return (rail?.publisher || rail?.ship || undefined);
    }
    return undefined;
}
//# sourceMappingURL=lib.js.map
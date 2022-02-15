
const ua = window.navigator.userAgent;

export const IS_IOS = ua.includes('iPhone') || ua.includes('iPad');

export const IS_SAFARI = ua.includes('Safari') && !ua.includes('Chrome');

export const IS_ANDROID = ua.includes('Android');

export const IS_MOBILE = (IS_IOS || IS_ANDROID) && window.innerWidth < 750;

export const isMobileApp = () => IS_MOBILE && window.location.search.includes('isMobileApp=true');
export const isMobileWeb = () => IS_MOBILE && !window.location.search.includes('isMobileApp=true');


const ua = window.navigator.userAgent;

export const IS_IOS = ua.includes('iPhone');

export const IS_SAFARI = ua.includes('Safari') && !ua.includes('Chrome');

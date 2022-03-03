export const DEFAULT_SESSION = '';

/**
 * Session ID validity:
 * 
 * - must start with an alphabetical
 * - can be composed of alphanumerics with hyphens
 * - can be length 1 or longer
 * - cannot begin or end with a hyphen
 */
export const SESSION_ID_REGEX = /(^[a-z]{1}[a-z\d\-]*[a-z\d]{1}$)|(^[a-z]{1}$)/;

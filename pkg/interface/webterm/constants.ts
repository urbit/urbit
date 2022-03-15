export const DEFAULT_SESSION = '';
export const DEFAULT_HANDLER = 'hood';

/**
 * Session ID validity:
 *
 * - must start with an alphabetical
 * - can be composed of alphanumerics with hyphens
 * - can be length 1 or longer
 */
export const SESSION_ID_REGEX = /(^[a-z]{1}[a-z\d-]*$)/;

/**
 * Open a session with a given agent using `[agent]![session_name]`
 *
 * For example:
 * ```
 * book!my-session
 * ```
 *
 * This will create a new session in webterm for the `%book` agent.
 *
 * Note that the second capture group after the ! is composed of the session ID
 * regex above.
 */
export const AGENT_SESSION_REGEX = /^([a-z]{1}[a-z\d-]*)!([a-z]{1}[a-z\d-]*$)/;

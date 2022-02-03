import { Scry, Thread, AuthenticationInterface, PokeInterface, SubscriptionRequestInterface } from './types';
/**
 * A class for interacting with an urbit ship, given its URL and code
 */
export declare class Urbit {
    url: string;
    code?: string;
    desk?: string;
    /**
     * UID will be used for the channel: The current unix time plus a random hex string
     */
    private uid;
    /**
     * Last Event ID is an auto-updated index of which events have been sent over this channel
     */
    private lastEventId;
    private lastAcknowledgedEventId;
    /**
     * SSE Client is null for now; we don't want to start polling until it the channel exists
     */
    private sseClientInitialized;
    /**
     * Cookie gets set when we log in.
     */
    cookie?: string | undefined;
    /**
     * A registry of requestId to successFunc/failureFunc
     *
     * These functions are registered during a +poke and are executed
     * in the onServerEvent()/onServerError() callbacks. Only one of
     * the functions will be called, and the outstanding poke will be
     * removed after calling the success or failure function.
     */
    private outstandingPokes;
    /**
     * A registry of requestId to subscription functions.
     *
     * These functions are registered during a +subscribe and are
     * executed in the onServerEvent()/onServerError() callbacks. The
     * event function will be called whenever a new piece of data on this
     * subscription is available, which may be 0, 1, or many times. The
     * disconnect function may be called exactly once.
     */
    private outstandingSubscriptions;
    /**
     * Our abort controller, used to close the connection
     */
    private abort;
    /**
     * Ship can be set, in which case we can do some magic stuff like send chats
     */
    ship?: string | null;
    /**
     * If verbose, logs output eagerly.
     */
    verbose?: boolean;
    /**
     * number of consecutive errors in connecting to the eventsource
     */
    private errorCount;
    onError?: (error: any) => void;
    onRetry?: () => void;
    onOpen?: () => void;
    /** This is basic interpolation to get the channel URL of an instantiated Urbit connection. */
    private get channelUrl();
    private get fetchOptions();
    /**
     * Constructs a new Urbit connection.
     *
     * @param url  The URL (with protocol and port) of the ship to be accessed. If
     * the airlock is running in a webpage served by the ship, this should just
     * be the empty string.
     * @param code The access code for the ship at that address
     */
    constructor(url: string, code?: string, desk?: string);
    /**
     * All-in-one hook-me-up.
     *
     * Given a ship, url, and code, this returns an airlock connection
     * that is ready to go. It `|hi`s itself to create the channel,
     * then opens the channel via EventSource.
     *
     */
    static authenticate({ ship, url, code, verbose, }: AuthenticationInterface): Promise<Urbit>;
    /**
     * Connects to the Urbit ship. Nothing can be done until this is called.
     * That's why we roll it into this.authenticate
     */
    connect(): Promise<void>;
    /**
     * Initializes the SSE pipe for the appropriate channel.
     */
    eventSource(): Promise<void>;
    /**
     * Reset airlock, abandoning current subscriptions and wiping state
     *
     */
    reset(): void;
    /**
     * Autoincrements the next event ID for the appropriate channel.
     */
    private getEventId;
    /**
     * Acknowledges an event.
     *
     * @param eventId The event to acknowledge.
     */
    private ack;
    private sendJSONtoChannel;
    /**
     * Creates a subscription, waits for a fact and then unsubscribes
     *
     * @param app Name of gall agent to subscribe to
     * @param path Path to subscribe to
     * @param timeout Optional timeout before ending subscription
     *
     * @returns The first fact on the subcription
     */
    subscribeOnce<T = any>(app: string, path: string, timeout?: number): Promise<T>;
    /**
     * Pokes a ship with data.
     *
     * @param app The app to poke
     * @param mark The mark of the data being sent
     * @param json The data to send
     */
    poke<T>(params: PokeInterface<T>): Promise<number>;
    /**
     * Subscribes to a path on an app on a ship.
     *
     *
     * @param app The app to subsribe to
     * @param path The path to which to subscribe
     * @param handlers Handlers to deal with various events of the subscription
     */
    subscribe(params: SubscriptionRequestInterface): Promise<number>;
    /**
     * Unsubscribes to a given subscription.
     *
     * @param subscription
     */
    unsubscribe(subscription: number): Promise<void>;
    /**
     * Deletes the connection to a channel.
     */
    delete(): void;
    /**
     * Scry into an gall agent at a path
     *
     * @typeParam T - Type of the scry result
     *
     * @remarks
     *
     * Equivalent to
     * ```hoon
     * .^(T %gx /(scot %p our)/[app]/(scot %da now)/[path]/json)
     * ```
     * The returned cage must have a conversion to JSON for the scry to succeed
     *
     * @param params The scry request
     * @returns The scry result
     */
    scry<T = any>(params: Scry): Promise<T>;
    /**
     * Run a thread
     *
     *
     * @param inputMark   The mark of the data being sent
     * @param outputMark  The mark of the data being returned
     * @param threadName  The thread to run
     * @param body        The data to send to the thread
     * @returns  The return value of the thread
     */
    thread<R, T = any>(params: Thread<T>): Promise<R>;
    /**
     * Utility function to connect to a ship that has its *.arvo.network domain configured.
     *
     * @param name Name of the ship e.g. zod
     * @param code Code to log in
     */
    static onArvoNetwork(ship: string, code: string): Promise<Urbit>;
}
export default Urbit;

import { Poke, Serial } from "../lib";
import { InviteUpdate, InviteUpdateAccept, InviteUpdateDecline } from "./types";
export declare const inviteAction: <T extends InviteUpdate>(data: T) => Poke<T>;
export declare const accept: (app: string, uid: Serial) => Poke<InviteUpdateAccept>;
export declare const decline: (app: string, uid: Serial) => Poke<InviteUpdateDecline>;

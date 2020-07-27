import { Path, Patp } from './noun';

export type ChatUpdate =
  ChatUpdateInitial
| ChatUpdateCreate
| ChatUpdateDelete
| ChatUpdateMessage
| ChatUpdateMessages
| ChatUpdateRead;

export type ChatAction =
  ChatUpdateCreate
| ChatUpdateDelete
| ChatUpdateMessage
| ChatUpdateRead;

interface ChatUpdateInitial {
  initial: Inbox;
}

interface ChatUpdateCreate {
  create: Path;
}

interface ChatUpdateDelete {
  delete: Path;
}

interface ChatUpdateMessage {
  message: {
    path: Path;
    envelope: Envelope;
  }
}

interface ChatUpdateMessages {
  messages: {
    path: Path;
    envelopes: Envelope[];
  }
}

interface ChatUpdateRead {
  read: {
    path: Path;
  };
}



// Data structures
// TODO: move to seperate file?

export interface Inbox {
  [chatName: string]: Mailbox;
}

interface Mailbox {
  config: MailboxConfig;
  envelopes: Envelope[];
}

interface MailboxConfig {
  length: number;
  read: number;
}

export interface Envelope {
  uid: string;
  number: number;
  author: Patp;
  when: string;
  letter: Letter;
}

interface LetterText {
  text: string;
}

interface LetterUrl {
  url: string;
}

interface LetterCode {
  code: {
    expression: string;
    output: string;
  }
}

interface LetterMe {
  narrative: string;
}

export type Letter = LetterText | LetterUrl | LetterCode | LetterMe;

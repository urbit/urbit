import { Patp, PatpNoSig, Path } from './noun';


export type NoteId = string;
export type BookId = string;


export type PublishUpdate =
  PublishUpdateAddBook
| PublishUpdateAddNote
| PublishUpdateAddComment
| PublishUpdateEditBook
| PublishUpdateEditNote
| PublishUpdateEditComment
| PublishUpdateDelBook
| PublishUpdateDelNote
| PublishUpdateDelComment;


type PublishUpdateBook = {
  [s in Patp]: {
    [b in BookId]: {
      title: string;
      'date-created': number;
      about: string;
      'num-notes': number;
      'num-unread': number;
      comments: boolean;
      'writers-group-path': Path;
      'subscribers-group-path': Path;
    };
  };
}

type PublishUpdateNote = {
  [s in Patp]: {
    [b in BookId]: {
      'note-id': NoteId;
      author: Patp;
      title: string;
      'date-created': string;
      snippet: string;
      file: string;
      'num-comments': number;
      comments: Comment[];
      read: boolean;
      pending: boolean;
    };
  };
};

interface PublishUpdateAddBook {
  'add-book': PublishUpdateBook;
}

interface PublishUpdateEditBook {
  'edit-book': PublishUpdateBook;
}

interface PublishUpdateDelBook {
  'del-book': {
    host: Patp;
    book: string;
  }
}

interface PublishUpdateAddNote {
  'add-note':  PublishUpdateNote;
}

interface PublishUpdateEditNote {
  'edit-note': PublishUpdateNote;
}

interface PublishUpdateDelNote {
  'del-note': {
    host: Patp;
    book: BookId;
    note: NoteId;
  }
}

interface PublishUpdateAddComment {
  'add-comment': {
    who: Patp;
    host: BookId;
    note: NoteId;
    body: string;
  }
}

interface PublishUpdateEditComment {
  'edit-comment': {
    host: Patp;
    book: BookId;
    note: NoteId;
    body: string;
    comment: Comment;
  }
}

interface PublishUpdateDelComment {
  'del-comment': {
    host: Patp;
    book: BookId;
    note: NoteId;
    comment: string;
  }
}

export type Notebooks = {
  [host in Patp]: {
    [book in BookId]: Notebook;
  }
}


export interface Notebook {
  about: string;
  comments: boolean;
  'date-created': number;
  notes: Notes;
  'notes-by-date': NoteId[];
  'num-notes': number;
  'num-unread': number;
  subscribers: PatpNoSig[];
  'subscribers-group-path': Path;
  title: string;
  'writers-group-path': Path;
}

type Notes = {
  [id in NoteId]: Note;
};

export interface Note {
  author: Patp;
  comments: Comment[];
  'date-created': number;
  file: string;
  'next-note': NoteId | null;
  'note-id': NoteId;
  'num-comments': number;
  pending: boolean;
  'prev-note': NoteId | null;
  read: boolean;
  snippet: string;
  title: string;
}

interface Comment {
  [date: string]: {
    author: Patp;
    content: string;
    'date-created': number;
    pending: boolean;
  };
}

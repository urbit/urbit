import BaseApi from './base';

import { PublishResponse } from '~/types/publish-response';
import { PatpNoSig, Path } from '~/types/noun';
import { BookId, NoteId } from '~/types/publish-update';

export default class PublishApi extends BaseApi {
  handleEvent(data: PublishResponse) {
    this.store.handleEvent({ data: { 'publish-response' : data } });
  }

  fetchNotebooks() {
    return fetch('/publish-view/notebooks.json')
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'notebooks',
        data: json
      });
    });
  }

  fetchNotebook(host: PatpNoSig, book: BookId) {
    return fetch(`/publish-view/${host}/${book}.json`)
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'notebook',
        data: json,
        host: host,
        notebook: book
      });
    });
  }

  fetchNote(host: PatpNoSig, book: BookId, note: NoteId) {
    return fetch(`/publish-view/${host}/${book}/${note}.json`)
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'note',
        data: json,
        host: host,
        notebook: book,
        note: note
      });
    });
  }

  fetchNotesPage(host: PatpNoSig, book: BookId, start: number, length: number) {
    return fetch(`/publish-view/notes/${host}/${book}/${start}/${length}.json`)
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'notes-page',
        data: json,
        host: host,
        notebook: book,
        startIndex: start,
        length: length
      });
    });
  }

  fetchCommentsPage(host: PatpNoSig, book: BookId, note: NoteId, start: number, length: number) {
    return fetch(`/publish-view/comments/${host}/${book}/${note}/${start}/${length}.json`)
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'comments-page',
        data: json,
        host: host,
        notebook: book,
        note: note,
        startIndex: start,
        length: length
      });
    });
  }

  subscribeNotebook(who: PatpNoSig, book: BookId) {
    return this.publishAction({
      subscribe: {
        who,
        book
      }
    });
  }

  unsubscribeNotebook(who: PatpNoSig, book: BookId) {
    return this.publishAction({
      unsubscribe: {
        who,
        book
      }
    });
  }

  publishAction(act: any) {
    return this.action('publish', 'publish-action', act);
  }

  groupify(bookId: string, group: Path | null) {
    return this.publishAction({
      groupify: {
        book: bookId,
        target: group,
        inclusive: false
      }
    });
  }


  newBook(bookId: string, title: string, description: string, group?: Path) {
    const groupInfo = group ? { 'group-path': group,
      invitees: [],
      'use-preexisting': true,
      'make-managed': true
    } : {
      'group-path': `/ship/~${window.ship}/${bookId}`,
      invitees: [],
      'use-preexisting': false,
      'make-managed': false
    };
    return this.publishAction({
      "new-book": {
        book: bookId,
        title: title,
        about: description,
        coms: true,
        group: groupInfo
      }
    });
  }

  editBook(bookId: string, title: string, description: string, coms: boolean) {
    return this.publishAction({
      "edit-book": {
        book: bookId,
        title: title,
        about: description,
        coms,
        group: null
      }
    });
  }

  delBook(book: string) {
    return this.publishAction({
      "del-book": {
        book
      }
    });
  }

  newNote(who: PatpNoSig, book: string, note: string, title: string, body: string) {
    return this.publishAction({
      'new-note': {
        who,
        book,
        note,
        title,
        body
      }
    });
  }

  editNote(who: PatpNoSig, book: string, note: string, title: string, body: string) {
    return this.publishAction({
      'edit-note': {
        who,
        book,
        note,
        title,
        body
      }
    });
  }

  delNote(who: PatpNoSig, book: string, note: string) {
    return this.publishAction({
      'del-note': {
        who,
        book,
        note
      }
    });
  }

  readNote(who: PatpNoSig, book: string, note: string) {
    return this.publishAction({
      read: {
        who,
        book,
        note
      }
    });
  }

  updateComment(who: PatpNoSig, book: string, note: string, comment: Path, body: string) {
    return this.publishAction({
      'edit-comment': {
        who,
        book,
        note,
        comment,
        body
      }
    });
  }

  deleteComment(who: PatpNoSig, book: string, note: string, comment: Path ) {
    return this.publishAction({
      "del-comment": {
        who,
        book,
        note,
        comment
      },
    });
  }

}


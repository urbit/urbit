import BaseApi from './base';
import { PublishResponse } from '../types/publish-response';
import { PatpNoSig } from '../types/noun';
import { BookId, NoteId } from '../types/publish-update';

export default class PublishApi extends BaseApi {
  handleEvent(data: PublishResponse) {
    this.store.handleEvent({ data: { 'publish-response' : data } });
  }

  fetchNotebooks() {
    fetch('/publish-view/notebooks.json')
    .then(response => response.json())
    .then((json) => {
      this.handleEvent({
        type: 'notebooks',
        data: json
      });
    });
  }

  fetchNotebook(host: PatpNoSig, book: BookId) {
    fetch(`/publish-view/${host}/${book}.json`)
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
    fetch(`/publish-view/${host}/${book}/${note}.json`)
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
    fetch(`/publish-view/notes/${host}/${book}/${start}/${length}.json`)
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
    fetch(`/publish-view/comments/${host}/${book}/${note}/${start}/${length}.json`)
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

  publishAction(act: any) {
    return this.action('publish', 'publish-action', act);
  }
}


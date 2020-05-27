import BaseApi from './base';


export default class PublishApi extends BaseApi {
  fetchNotebooks() {
    fetch('/publish-view/notebooks.json')
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
        type: 'notebooks',
        data: json
      });
    });
  }

  fetchNotebook(host, book) {
    fetch(`/publish-view/${host}/${book}.json`)
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
        type: 'notebook',
        data: json,
        host: host,
        notebook: book
      });
    });
  }

  fetchNote(host, book, note) {
    fetch(`/publish-view/${host}/${book}/${note}.json`)
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
        type: 'note',
        data: json,
        host: host,
        notebook: book,
        note: note
      });
    });
  }

  fetchNotesPage(host, book, start, length) {
    fetch(`/publish-view/notes/${host}/${book}/${start}/${length}.json`)
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
        type: 'notes-page',
        data: json,
        host: host,
        notebook: book,
        startIndex: start,
        length: length
      });
    });
  }

  fetchCommentsPage(host, book, note, start, length) {
    fetch(`/publish-view/comments/${host}/${book}/${note}/${start}/${length}.json`)
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
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

  sidebarToggle() {
    let sidebarBoolean = true;
    if (this.store.state.sidebarShown === true) {
      sidebarBoolean = false;
    }
    this.store.handleEvent({
      type: 'local',
      data: {
        'sidebarToggle': sidebarBoolean
      }
    });
  }
}


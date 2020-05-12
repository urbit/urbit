import _ from 'lodash';

export default class Api {
  constructor(ship, channel, store) {
    this.ship = ship;
    this.channel = channel;
    this.store = store;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.ship, appl = 'publish', success, fail) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

   this.channel.subscribe(ship, appl, path,
      (err) => {
        fail(err);
      },
      (event) => {
        success({
          data: event,
          from: {
            ship,
            path
          }
        });
      },
      (err) => {
        fail(err);
      }
    );
  }

  action(appl, mark, data) {
    return new Promise((resolve, reject) => {
      this.channel.poke(this.ship, appl, mark, data,
        (json) => {
          resolve(json);
        },
        (err) => {
          reject(err);
        });
    });
  }

  // TODO add error handling

  handleErrors(response) {
    if (!response.ok)
throw Error(response.status);
    return response;
  }

  fetchNotebooks() {
    fetch('/~publish/notebooks.json')
    .then(response => response.json())
    .then((json) => {
      this.store.handleEvent({
        type: 'notebooks',
        data: json
      });
    });
  }

  fetchNotebook(host, book) {
    fetch(`/~publish/${host}/${book}.json`)
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
    fetch(`/~publish/${host}/${book}/${note}.json`)
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
    fetch(`/~publish/notes/${host}/${book}/${start}/${length}.json`)
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
    fetch(`/~publish/comments/${host}/${book}/${note}/${start}/${length}.json`)
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

  setSelected(selected) {
    this.store.handleEvent({
      type: 'local',
      data: {
        selected: selected
      }
    });
  }
}

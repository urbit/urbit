import _ from 'lodash';

export class PrimaryReducer {
  reduce(json, state){
    switch(Object.keys(json)[0]){
      //publish actions
      case "add-book":
        this.addBook(json["add-book"], state);
        break;
      case "add-note":
        this.addNote(json["add-note"], state);
        break;
      case "add-comment":
        this.addComment(json["add-comment"], state);
        break;
      case "edit-book":
        this.editBook(json["edit-book"], state);
        break;
      case "edit-note":
        this.editNote(json["edit-note"], state);
        break;
      case "edit-comment":
        this.editComment(json["edit-comment"], state);
        break;
      case "del-book":
        this.delBook(json["del-book"], state);
        break;
      case "del-note":
        this.delNote(json["del-note"], state);
        break;
      case "del-comment":
        this.delComment(json["del-comment"], state);
        break;
      case "read":
        this.read(json["read"], state);
        break;
      // contacts actions
      case "contact-initial":
        this.contactInitial(json["contact-initial"], state);
        break;
      case "contact-update":
        this.contactUpdate(json["contact-update"], state);
      default:
        break;
    }
  }

  addBook(json, state) {
    let host = Object.keys(json)[0];
    let book = Object.keys(json[host])[0];
    if (state.notebooks[host]) {
      state.notebooks[host][book] = json[host][book];
    } else {
      state.notebooks[host] = json[host];
    }
  }

  addNote(json, state) {
    let host   = Object.keys(json)[0];
    let book   = Object.keys(json[host])[0];
    let noteId = json[host][book]["note-id"];
    if (state.notebooks[host] && state.notebooks[host][book]) {
      if (state.notebooks[host][book]["notes-by-date"]) {
        state.notebooks[host][book]["notes-by-date"].unshift(noteId);
      } else {
        state.notebooks[host][book]["notes-by-date"] = [noteId];
      }

      if (state.notebooks[host][book].notes) {
        state.notebooks[host][book].notes[noteId] = json[host][book];
      } else {
        state.notebooks[host][book].notes = {[noteId]: json[host][book]};
      }

      state.notebooks[host][book]["num-notes"] += 1;
      if (!json[host][book].read) {
        state.notebooks[host][book]["num-unread"] += 1;
      }
      let prevNoteId = state.notebooks[host][book]["notes-by-date"][1] || null;
      state.notebooks[host][book].notes[noteId]["prev-note"] = prevNoteId
      state.notebooks[host][book].notes[noteId]["next-note"] = null;

      if (state.notebooks[host][book].notes[prevNoteId]) {
        state.notebooks[host][book].notes[prevNoteId]["next-note"] = noteId;
      }
    }
  }

  addComment(json, state) {
    let host    = json.host
    let book    = json.book
    let note    = json.note
    let comment = json.comment;
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes &&
        state.notebooks[host][book].notes[note])
    {
      state.notebooks[host][book].notes[note]["num-comments"] += 1;
      if (state.notebooks[host][book].notes[note].comments) {
        state.notebooks[host][book].notes[note].comments.unshift(comment);
      } else if (state.notebooks[host][book].notes[note]["num-comments"] === 1) {
        state.notebooks[host][book].notes[note].comments = [comment];
      }
    }
  }

  editBook(json, state) {
    let host = Object.keys(json)[0];
    let book = Object.keys(json[host])[0];
    if (state.notebooks[host] && state.notebooks[host][book]) {
      state.notebooks[host][book]["date-created"] = json[host][book]["date-created"];
      state.notebooks[host][book]["num-notes"] = json[host][book]["num-notes"];
      state.notebooks[host][book]["num-unread"] = json[host][book]["num-unread"];
      state.notebooks[host][book]["title"] = json[host][book]["title"];
    }
  }

  editNote(json, state) {
    let host   = Object.keys(json)[0];
    let book   = Object.keys(json[host])[0];
    let noteId = json[host][book]["note-id"];
    let note   = json[host][book];
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes &&
        state.notebooks[host][book].notes[noteId])
    {
      state.notebooks[host][book].notes[noteId]["author"] = note["author"];
      state.notebooks[host][book].notes[noteId]["build"] = note["build"];
      state.notebooks[host][book].notes[noteId]["file"] = note["file"];
      state.notebooks[host][book].notes[noteId]["title"] = note["title"];
    }
  }

  editComment(json, state) {
    let host      = json.host
    let book      = json.book
    let note      = json.note
    let comment   = json.comment;
    let commentId = Object.keys(comment)[0]
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes &&
        state.notebooks[host][book].notes[note] &&
        state.notebooks[host][book].notes[note].comments)
    {
      let keys = state.notebooks[host][book].notes[note].comments.map((com) => {
        return Object.keys(com)[0];
      });
      let index = keys.indexOf(commentId);
      if (index > -1) {
        state.notebooks[host][book].notes[note].comments[index] = comment;
      }
    }
  }

  delBook(json, state) {
    let host = json.host;
    let book = json.book;
    if (state.notebooks[host]) {
      if (state.notebooks[host][book]) {
        delete state.notebooks[host][book];
      }
      if (Object.keys(state.notebooks[host]).length === 0) {
        delete state.notebooks[host];
      }
    }
  }

  delNote(json, state) {
    let host = json.host;
    let book = json.book;
    let note = json.note;
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes)
    {
      if (state.notebooks[host][book].notes[note]) {
        state.notebooks[host][book]["num-notes"] -= 1;
        if (!state.notebooks[host][book].notes[note].read) {
          state.notebooks[host][book]["num-unread"] -= 1;
        }

        delete state.notebooks[host][book].notes[note];
        let index = state.notebooks[host][book]["notes-by-date"].indexOf(note);
        if (index > -1) {
          state.notebooks[host][book]["notes-by-date"].splice(index, 1);
        }

      }
      if (Object.keys(state.notebooks[host][book].notes).length === 0) {
        delete state.notebooks[host][book].notes;
        delete state.notebooks[host][book]["notes-by-date"];
      }
    }
  }

  delComment(json, state) {
    let host    = json.host
    let book    = json.book
    let note    = json.note
    let comment = json.comment;
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes &&
        state.notebooks[host][book].notes[note])
    {
      state.notebooks[host][book].notes[note]["num-comments"] -= 1;
      if (state.notebooks[host][book].notes[note].comments) {
        let keys = state.notebooks[host][book].notes[note].comments.map((com) => {
          return Object.keys(com)[0];
        });

        let index = keys.indexOf(comment);
        if (index > -1) {
          state.notebooks[host][book].notes[note].comments.splice(index, 1);
        }
      }
    }
  }

  read(json, state){
    let host   = json.host;
    let book   = json.book;
    let noteId = json.note
    if (state.notebooks[host] &&
        state.notebooks[host][book] &&
        state.notebooks[host][book].notes &&
        state.notebooks[host][book].notes[noteId])
    {
      state.notebooks[host][book].notes[noteId]["read"] = true;
    }
  }

  contactInitial(json, state) {
    state.contacts = json;
  }

  contactUpdate(json, state) {
    this.createContact(json, state);
    this.deleteContact(json, state);
    this.addContact(json, state);
    this.removeContact(json, state);
    this.editContact(json, state);
  }

  createContact(json, state) {
    let data = _.get(json, "create", false);
    if (data) {
      state.contacts[data.path] = {};
    }
  }

  deleteContact(json, state) {
    let data = _.get(json, "delete", false);
    if (data) {
      delete state.contacts[data.path];
    }
  }

  addContact(json, state) {
    let data = _.get(json, "add", false);
    if (data && data.path in state.contacts) {
      state.contacts[data.path][data.ship] = data.contact;
    }
  }

  removeContact(json, state) {
    let data = _.get(json, "remove", false);
    if (
      data &&
      data.path in state.contacts &&
      data.ship in state.contacts[data.path]
    ) {
      delete state.contacts[data.path][data.ship];
    }
  }

  editContact(json, state) {
    let data = _.get(json, "edit", false);
    if (
      data &&
      data.path in state.contacts &&
      data.ship in state.contacts[data.path]
    ) {
      let edit = Object.keys(data["edit-field"]);
      if (edit.length !== 1) {
        return;
      }
      state.contacts[data.path][data.ship][edit[0]] =
        data["edit-field"][edit[0]];
    }
  }
}
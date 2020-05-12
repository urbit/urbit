import _ from 'lodash';

export class ResponseReducer {
  reduce(json, state) {
    switch(json.type) {
      case "notebooks":
        this.handleNotebooks(json, state);
        break;
      case "notebook":
        this.handleNotebook(json, state);
        break;
      case "note":
        this.handleNote(json, state);
        break;
      case "notes-page":
        this.handleNotesPage(json, state);
        break;
      case "comments-page":
        this.handleCommentsPage(json, state);
        break;
      case "local":
        this.sidebarToggle(json, state);
        this.setSelected(json, state);
        break;
      default:
        break;
    }
  }

  handleNotebooks(json, state) {
    for (var host in state.notebooks) {
      if (json.data[host]) {
        for (var book in state.notebooks[host]) {
          if (!json.data[host][book]) {
            delete state.notebooks[host][book];
          }
        }
      } else {
        delete state.notebooks[host];
      }
    }

    for (var host in json.data) {
      if (state.notebooks[host]) {
        for (var book in json.data[host]) {
          if (state.notebooks[host][book]) {
            state.notebooks[host][book]["title"] = json.data[host][book]["title"];
            state.notebooks[host][book]["date-created"] =
              json.data[host][book]["date-created"];
            state.notebooks[host][book]["num-notes"] =
              json.data[host][book]["num-notes"];
            state.notebooks[host][book]["num-unread"] =
              json.data[host][book]["num-unread"];
          } else {
            state.notebooks[host][book] = json.data[host][book];
          }
        }
      } else {
        state.notebooks[host] = json.data[host];
      }
    }
  }

  handleNotebook(json, state) {
    if (state.notebooks[json.host]) {
      if (state.notebooks[json.host][json.notebook]) {
        state.notebooks[json.host][json.notebook]["notes-by-date"] =
          json.data.notebook["notes-by-date"];
        state.notebooks[json.host][json.notebook].subscribers =
          json.data.notebook.subscribers;
        state.notebooks[json.host][json.notebook].comments =
          json.data.notebook.comments;
        state.notebooks[json.host][json.notebook]["subscribers-group-path"] =
          json.data.notebook["subscribers-group-path"];
        state.notebooks[json.host][json.notebook]["writers-group-path"] =
          json.data.notebook["writers-group-path"];
        if (state.notebooks[json.host][json.notebook].notes) {
          for (var key in json.data.notebook.notes) {
            let oldNote = state.notebooks[json.host][json.notebook].notes[key];
            if (!(oldNote)) {
              state.notebooks[json.host][json.notebook].notes[key] =
                json.data.notebook.notes[key];
            } else if (!(oldNote.build)) {
              state.notebooks[json.host][json.notebook].notes[key]["author"] =
                json.data.notebook.notes[key]["author"];
              state.notebooks[json.host][json.notebook].notes[key]["date-created"] =
                json.data.notebook.notes[key]["date-created"];
              state.notebooks[json.host][json.notebook].notes[key]["note-id"] =
                json.data.notebook.notes[key]["note-id"];
              state.notebooks[json.host][json.notebook].notes[key]["num-comments"] =
                json.data.notebook.notes[key]["num-comments"];
              state.notebooks[json.host][json.notebook].notes[key]["title"] =
                json.data.notebook.notes[key]["title"];
            }
          }
        } else {
          state.notebooks[json.host][json.notebook].notes =
            json.data.notebook.notes;
        }
      } else {
        state.notebooks[json.host][json.notebook] = json.data.notebook;
      }
    } else {
      state.notebooks[json.host] = {[json.notebook]: json.data.notebook};
    }
  }

  handleNote(json, state) {
    if (state.notebooks[json.host] &&
        state.notebooks[json.host][json.notebook]) {
      state.notebooks[json.host][json.notebook]["notes-by-date"] =
        json.data["notes-by-date"];
      if (state.notebooks[json.host][json.notebook].notes) {
        for (var key in json.data.notes) {
          let oldNote = state.notebooks[json.host][json.notebook].notes[key];
          if (!(oldNote && oldNote.build && key !== json.note)) {
            state.notebooks[json.host][json.notebook].notes[key] =
              json.data.notes[key];
          }
        }
      } else {
        state.notebooks[json.host][json.notebook].notes = json.data.notes;
      }
    } else {
      throw Error("tried to fetch note, but we don't have the notebook");
    }
  }

  handleNotesPage(json, state) {
    if (state.notebooks[json.host] && state.notebooks[json.host][json.notebook]) {
      state.notebooks[json.host][json.notebook]["notes-by-date"] =
        json.data["notes-by-date"];
      if (state.notebooks[json.host][json.notebook].notes) {
        for (var key in json.data.notes) {
          let oldNote = state.notebooks[json.host][json.notebook].notes[key];
          if (!(oldNote)) {
            state.notebooks[json.host][json.notebook].notes[key] =
              json.data.notes[key];
          } else if (!(oldNote.build)) {
            state.notebooks[json.host][json.notebook].notes[key]["author"] =
              json.data.notes[key]["author"];
            state.notebooks[json.host][json.notebook].notes[key]["date-created"] =
              json.data.notes[key]["date-created"];
            state.notebooks[json.host][json.notebook].notes[key]["note-id"] =
              json.data.notes[key]["note-id"];
            state.notebooks[json.host][json.notebook].notes[key]["num-comments"] =
              json.data.notes[key]["num-comments"];
            state.notebooks[json.host][json.notebook].notes[key]["title"] =
              json.data.notes[key]["title"];
          }
        }
      } else {
        state.notebooks[json.host][json.notebook].notes =
          json.data.notes;
      }
    } else {
      throw Error("tried to fetch paginated notes, but we don't have the notebook");
    }
  }

  handleCommentsPage(json, state) {
    if (state.notebooks[json.host] &&
        state.notebooks[json.host][json.notebook] &&
        state.notebooks[json.host][json.notebook].notes[json.note])
    {
      if (state.notebooks[json.host][json.notebook].notes[json.note].comments) {
        json.data.forEach((val, i) => {
          let newKey = Object.keys(val)[0];
          let newDate = val[newKey]["date-created"]
          let oldComments = state.notebooks[json.host][json.notebook].notes[json.note].comments;
          let insertIdx = -1;

          for (var j=0; j<oldComments.length; j++) {
            let oldKey = Object.keys(oldComments[j])[0];
            let oldDate = oldComments[j][oldKey]["date-created"];

            if (oldDate === newDate) {
              break;
            } else if (oldDate < newDate) {
              insertIdx = j;
            } else if ((oldDate > newDate) &&
                      (j === oldComments.length-1)){
              insertIdx = j+1;
            }
          }
          if (insertIdx !== -1) {
            state.notebooks[json.host][json.notebook].notes[json.note].comments
            .splice(insertIdx, 0, val);
          }
        });
      } else {
        state.notebooks[json.host][json.notebook].notes[json.note].comments =
          json.data;
      }
    } else {
      throw Error("tried to fetch paginated comments, but we don't have the note");
    }
  }

  sidebarToggle(json, state) {
    let data = _.has(json.data, 'sidebarToggle', false);
    if (data) {
        state.sidebarShown = json.data.sidebarToggle;
    }
  }

  setSelected(json, state) {
    let data = _.has(json.data, 'selected', false);
    if (data) {
      state.selectedGroups = json.data.selected;
    }
  }

}

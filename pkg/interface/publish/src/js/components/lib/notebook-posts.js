import React, { Component } from 'react';
import moment from 'moment';
import { Link } from 'react-router-dom';

export class NotebookPosts extends Component {
  constructor(props){
    super(props);
    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago'
        },
        s : 'just now',
        future : 'in %s',
        m  : '1m',
        mm : '%dm',
        h  : '1h',
        hh : '%dh',
        d  : '1d',
        dd : '%dd',
        M  : '1 month',
        MM : '%d months',
        y  : '1 year',
        yy : '%d years',
      }
    });
  }

  render() {
    let notes = [];

    for (var i=0; i<this.props.list.length; i++) {
      let noteId = this.props.list[i];
      let note = this.props.notes[noteId];
      if (!note) {
        break;
      }
      let comment = "No Comments";
      if (note["num-comments"] == 1) {
        comment = "1 Comment";
      } else if (note["num-comments"] > 1) {
        comment = `${note["num-comments"]} Comments`;
      }
      let date = moment(note["date-created"]).fromNow();
      let url = `/~publish/note/${this.props.host}/${this.props.notebookName}/${noteId}`

      notes.push(
        <Link key={i} to={url}>
          <div className="mv6">
            <div className="mb1">{note.title}</div>
            <p className="mb1">{note.snippet}</p>
            <div className="flex">
              <div className="mono gray2 mr3">{note.author}</div>
              <div className="gray2 mr3">{date}</div>
              <div className="gray2">{comment}</div>
            </div>
          </div>
        </Link>
      )
    }

    return (
      <div className="flex-col">
        {notes}
      </div>
    );

  }
}

export default NotebookPosts

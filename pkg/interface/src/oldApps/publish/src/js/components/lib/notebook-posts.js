import React, { Component } from 'react';
import moment from 'moment';
import { Link } from 'react-router-dom';
import ReactMarkdown from 'react-markdown'
import { cite } from '../../lib/util';

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
    const { props } = this;
    let notes = [];

    for (var i=0; i<props.list.length; i++) {
      let noteId = props.list[i];
      let note = props.notes[noteId];
      if (!note) {
        break;
      }

      let contact = !!(note.author.substr(1) in props.contacts)
        ? props.contacts[note.author.substr(1)] : false;

      let name = note.author;
      if (contact) {
        name = (contact.nickname.length > 0)
          ? contact.nickname : note.author;
      }
      if (name === note.author) {
        name = cite(note.author);
      }
      let comment = "No Comments";
      if (note["num-comments"] == 1) {
        comment = "1 Comment";
      } else if (note["num-comments"] > 1) {
        comment = `${note["num-comments"]} Comments`;
      }
      let date = moment(note["date-created"]).fromNow();
      let popout = (props.popout) ? "popout/" : "";
      let url = `/~publish/${popout}note/${props.host}/${props.notebookName}/${noteId}`

      notes.push(
        <Link key={i} to={url}>
          <div className="mv6">
            <div className="mb1"
            style={{overflowWrap: "break-word"}}>
              {note.title}
            </div>
            <p className="mb1"
            style={{overflowWrap: "break-word"}}>
              <ReactMarkdown
                unwrapDisallowed
                allowedTypes={['text', 'root', 'break', 'paragraph']}
                source={note.snippet} />
            </p>
            <div className="flex">
              <div className={(contact.nickname ? null : "mono") +
               " gray2 mr3"}
               title={note.author}>{name}</div>
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

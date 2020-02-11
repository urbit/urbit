import React, { Component } from 'react';
import { Comments } from './comments';
import { NoteNavigation } from './note-navigation';
import moment from 'moment';
import ReactMarkdown from 'react-markdown';
//TODO ask for note if we don't have it
//TODO initialise note if no state

//TODO if comments are disabled on the notebook, don't render comments
export class Note extends Component {
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
    this.scrollElement = React.createRef();
    this.onScroll = this.onScroll.bind(this);
  }

  componentWillMount() {
    window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
  }

  componentDidUpdate(prevProps) {
    if (!(this.props.notebooks[this.props.ship]) ||
        !(this.props.notebooks[this.props.ship][this.props.book]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note].file))
    {
      window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
    }
  }

  onScroll() {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    let note = notebook.notes[this.props.note];

    if (!note.comments) {
      return;
    }

    let scrollTop = this.scrollElement.scrollTop;
    let clientHeight = this.scrollElement.clientHeight;
    let scrollHeight = this.scrollElement.scrollHeight;

    let atBottom = false;
    if (scrollHeight - scrollTop - clientHeight < 40) {
      atBottom = true;
    }

    let loadedComments = note.comments.length;
    let allComments = note["num-comments"];

    let fullyLoaded = (loadedComments === allComments);

    if (atBottom && !fullyLoaded) {
      window.api.fetchCommentsPage(this.props.ship,
        this.props.book, this.props.note, loadedComments, 30);
    }
  }

  componentDidMount() {
    if (!(this.props.notebooks[this.props.ship]) ||
        !(this.props.notebooks[this.props.ship][this.props.book]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note].file))
    {
      window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
    }
    this.onScroll();
  }

  render() {
    const { props } = this;
    let notebook = props.notebooks[props.ship][props.book];
    let comments = notebook.notes[props.note].comments;
    let title = notebook.notes[props.note].title;
    let author = notebook.notes[props.note].author;
    let file = notebook.notes[props.note].file;
    let date = moment(notebook.notes[props.note]["date-created"]).fromNow();

    let contact = !!(author.substr(1) in props.contacts)
      ? props.contacts[author.substr(1)] : false;

    let name = author;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : author;
    }

    if (!file) {
      return null;
    }

    let newfile = file.slice(file.indexOf(';>')+2);
    let prevId = notebook.notes[props.note]["prev-note"];
    let nextId = notebook.notes[props.note]["next-note"];

    let prev = (prevId === null)
      ?  null
      :  {
        id: prevId,
        title: notebook.notes[prevId].title,
        date: moment(notebook.notes[prevId]["date-created"]).fromNow()
      }
      let next = (nextId === null)
        ?  null
        :  {
          id: nextId,
          title: notebook.notes[nextId].title,
          date: moment(notebook.notes[nextId]["date-created"]).fromNow()
        }


    return (
      <div className="h-100 overflow-container no-scrollbar"
           onScroll={this.onScroll}
           ref={(el) => {this.scrollElement = el}}>
        <div className="flex justify-center mt4 ph4 pb4">
          <div className="w-100 mw6">
            <div className="flex flex-column">
              <div className="f9 mb1">{title}</div>
              <div className="flex mb6">
                <div className={"di f9 gray2 mr2 " +
                ((name === author) ? "mono" : "")}>
                  {name}
                </div>
                <div className="di f9 gray2">{date}</div>
              </div>
            </div>
            <div className="md">
              <ReactMarkdown source={newfile} />
            </div>
            <NoteNavigation
              prev={prev}
              next={next}
              ship={props.ship}
              book={props.book}/>
            <Comments ship={props.ship}
              book={props.book}
              note={props.note}
              comments={comments}
              contacts={props.contacts}
              />
          </div>
        </div>
      </div>
    )
  }
}

export default Note

import React, { Component } from 'react';
import { Comments } from './comments';
import { NoteNavigation } from './note-navigation';
import moment from 'moment';
import ReactMarkdown from 'react-markdown'
// import test from 'test.json';
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
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    let comments = notebook.notes[this.props.note].comments;
    let title = notebook.notes[this.props.note].title;
    let author = notebook.notes[this.props.note].author;
    let file = notebook.notes[this.props.note].file;
    let date = moment(notebook.notes[this.props.note]["date-created"]).fromNow();

    if (!file) {
      return null;
    }

    let newfile = file.slice(file.indexOf(';>')+2);
    let prevId = notebook.notes[this.props.note]["prev-note"];
    let nextId = notebook.notes[this.props.note]["next-note"];

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
              <div className="flex">
              <div className="di f9 mono gray2 mr2">{author}</div>
              <div className="di f9 gray2">{date}</div>
              </div>
            </div>

            <ReactMarkdown source={newfile} />
            <NoteNavigation
              prev={prev}
              next={next}
              ship={this.props.ship}
              book={this.props.book}/>
            <Comments ship={this.props.ship}
              book={this.props.book}
              note={this.props.note}
              comments={comments}/>
          </div>
        </div>
      </div>
    )
  }
}

export default Note

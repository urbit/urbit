import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { SidebarSwitcher } from './icons/icon-sidebar-switch';
import { Spinner } from './icons/icon-spinner';
import { Comments } from './comments';
import { NoteNavigation } from './note-navigation';
import moment from 'moment';
import ReactMarkdown from 'react-markdown';
import { cite } from '../../lib/util';

export class Note extends Component {
  constructor(props) {
    super(props);
    this.state = {
      deleting: false
    }
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
    this.deletePost = this.deletePost.bind(this);
  }

  componentWillMount() {
    let readAction = {
      read: {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
      }
    }
    window.api.action("publish", "publish-action", readAction);
    window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
  }

  componentDidMount() {
    if (!(this.props.notebooks[this.props.ship]) ||
      !(this.props.notebooks[this.props.ship][this.props.book]) ||
      !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note]) ||
      !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note].file)) {
      window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
    }
    this.onScroll();
  }

  componentDidUpdate(prevProps) {
    if (!(this.props.notebooks[this.props.ship]) ||
        !(this.props.notebooks[this.props.ship][this.props.book]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note]) ||
        !(this.props.notebooks[this.props.ship][this.props.book].notes[this.props.note].file))
    {
      window.api.fetchNote(this.props.ship, this.props.book, this.props.note);
    }
    if ((prevProps.book !== this.props.book) ||
        (prevProps.note !== this.props.note) ||
        (prevProps.ship !== this.props.ship)) {
      let readAction = {
        read: {
          who: this.props.ship.slice(1),
          book: this.props.book,
          note: this.props.note,
        }
      }
      window.api.action("publish", "publish-action", readAction);
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

  deletePost() {
    const { props } = this;
    let deleteAction = {
      "del-note": {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
      }
    }
    let popout = (props.popout) ? "popout/" : "";
    let baseUrl = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    this.setState({deleting: true});
    window.api.action("publish", "publish-action", deleteAction)
    .then(() => {
      props.history.push(baseUrl);
    });
  }

  render() {
    const { props } = this;
    let notebook = props.notebooks[props.ship][props.book] || {};
    let comments = notebook.notes[props.note].comments || false;
    let title = notebook.notes[props.note].title || "";
    let author = notebook.notes[props.note].author || "";
    let file = notebook.notes[props.note].file || "";
    let date = moment(notebook.notes[props.note]["date-created"]).fromNow() || 0;

    let contact = !!(author.substr(1) in props.contacts)
      ? props.contacts[author.substr(1)] : false;

    let name = author;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : author;
    }

    if (name === author) {
      name = cite(author);
    }

    if (!file) {
      return null;
    }

    let newfile = file.slice(file.indexOf(';>')+2);
    let prevId = notebook.notes[props.note]["prev-note"] || null;
    let nextId = notebook.notes[props.note]["next-note"] || null;

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

    let editPost = null;
    let editUrl = props.location.pathname + "/edit";
    if (`~${window.ship}` === author) {
        editPost = <div className="dib">
        <Link className="green2 f9" to={editUrl}>Edit</Link>
        <p className="dib f9 red2 ml2 pointer"
          onClick={(() => this.deletePost())}>Delete</p>
        </div>
    }

    let popout = (props.popout) ? "popout/" : "";

    let hrefIndex = props.location.pathname.indexOf("/note/");
    let publishsubStr = props.location.pathname.substr(hrefIndex);
    let popoutHref = `/~publish/popout${publishsubStr}`;

    let hiddenOnPopout = props.popout ? "" : "dib-m dib-l dib-xl";

    let baseUrl = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    return (
      <div
        className="h-100 overflow-y-scroll"
        onScroll={this.onScroll}
        ref={el => {
          this.scrollElement = el;
        }}>
        <div className="h-100 flex flex-column items-center pa4">
          <div className="w-100 flex justify-center pb6">
            <SidebarSwitcher
              popout={props.popout}
              sidebarShown={props.sidebarShown}
            />
            <Link className="f9 w-100 w-90-m w-90-l mw6 tl" to={baseUrl}>
              {"<- Notebook index"}
            </Link>
            <Link
            to={popoutHref}
            className={"dn absolute right-1 top-1 " + hiddenOnPopout}
            target="_blank">
              <img src="/~publish/popout.png"
                height={16}
                width={16}
              />
            </Link>
          </div>
          <div className="w-100 mw6">
            <div className="flex flex-column">
              <div className="f9 mb1"
              style={{overflowWrap: "break-word"}}>{title}</div>
              <div className="flex mb6">
                <div
                  className={
                    "di f9 gray2 mr2 " + (contact.nickname ? null : "mono")
                  }
                  title={author}>
                  {name}
                </div>
                <div className="di">
                  <span className="f9 gray2 dib">{date}</span><span className="ml2 dib">{editPost}</span></div>
              </div>
            </div>
            <div className="md"
            style={{overflowWrap: "break-word"}}>
              <ReactMarkdown source={newfile} linkTarget={"_blank"} />
            </div>
            <NoteNavigation
              popout={props.popout}
              prev={prev}
              next={next}
              ship={props.ship}
              book={props.book}
            />
            <Comments enabled={notebook.comments}
              ship={props.ship}
              book={props.book}
              note={props.note}
              comments={comments}
              contacts={props.contacts}
            />
            <Spinner text="Deleting post..." awaiting={this.state.deleting} classes="absolute bottom-1 right-1 ba b--gray1-d pa2" />
          </div>
        </div>
      </div>
    );
  }
}

export default Note

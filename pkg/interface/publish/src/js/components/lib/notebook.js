import React, { Component } from 'react';
import { Link, Switch, Route } from 'react-router-dom';
import { NoteList } from './note-list';
import { NotebookPosts } from './notebook-posts';
import { About } from './about';
import { Subscribers } from './subscribers';
import { Settings } from './settings';

//TODO subcomponent logic for subscribers, settings

export class Notebook extends Component {
  constructor(props){
    super(props);

    this.onScroll = this.onScroll.bind(this);
    this.unsubscribe = this.unsubscribe.bind(this);
  }

  onScroll() {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    let scrollTop = this.scrollElement.scrollTop;
    let clientHeight = this.scrollElement.clientHeight;
    let scrollHeight = this.scrollElement.scrollHeight;

    let atBottom = false;
    if (scrollHeight - scrollTop - clientHeight < 40) {
      atBottom = true;
    }
    let loadedNotes = Object.keys(notebook.notes).length;
    let allNotes = notebook["notes-by-date"].length;

    let fullyLoaded = (loadedNotes === allNotes);

    if (atBottom && !fullyLoaded) {
      window.api.fetchNotesPage(this.props.ship, this.props.book, loadedNotes, 30);
    }
  }

  componentWillMount(){
    window.api.fetchNotebook(this.props.ship, this.props.book);
  }

  componentDidMount() {
    if (this.props.notebooks[this.props.ship][this.props.book].notes) {
      this.onScroll();
    }
  }

  unsubscribe() {
    let action = {
      unsubscribe: {
        who: this.props.ship.slice(1),
        book: this.props.book,
      }
    }
    window.api.action("publish", "publish-action", action);
    this.props.history.push("/~publish");
  }

  render() {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    let tabStyles = {
      posts: "bb b--gray4 gray2 pv4 ph2",
      about: "bb b--gray4 gray2 pv4 ph2"
      //      subscribers: "bb b--gray4 gray2 pv4 ph2",
      //      settings: "bb b--gray4 pr2 gray2 pv4 ph2",
    };
    tabStyles[this.props.view] = "bb b--black black pv4 ph2";

    let inner = null;
    switch (this.props.view) {
      case "posts":
        let notesList = notebook["notes-by-date"] || [];
        let notes = notebook.notes || null;
        inner = <NotebookPosts notes={notes}
                  list={notesList}
                  host={this.props.ship}
                  notebookName={this.props.book}/>
        break;
      case "about":
        inner = <p className="f8 lh-solid">{notebook.about}</p>
        break;
//      case "subscribers":
//        inner = <Subscribers/>
//        break;
//      case "settings":
//        inner = <Settings/>
//        break;
      default:
        break;
    }

    let base = `/~publish/notebook/${this.props.ship}/${this.props.book}`;
    let about = base + '/about';
    let subs = base + '/subscribers';
    let settings = base + '/settings';
    let newUrl = base + '/new';

    let newPost = !(window.ship === this.props.ship.slice(1))
      // XX TODO check based on authorized writers instead
      ?  null
      :  <Link to={newUrl} className="NotebookButton bg-light-green green2">
           New Post
         </Link>

    let unsub = (window.ship === this.props.ship.slice(1))
      ? null
      :  <button onClick={this.unsubscribe}
             className="NotebookButton bg-white black ba b--black ml3">
           Unsubscribe
         </button>

    return (
      <div
        className="center mw6 f9 h-100"
        style={{ paddingLeft: 16, paddingRight: 16 }}>
        <div
          className="h-100 overflow-container no-scrollbar"
          onScroll={this.onScroll}
          ref={el => {
            this.scrollElement = el;
          }}>
          <div
            className="flex justify-between"
            style={{ marginTop: 56, marginBottom: 32 }}>
            <div className="flex-col">
              <div className="mb1">{notebook.title}</div>
              <span>
                <span className="gray3 mr1">by</span>
                <span className="mono">{this.props.ship}</span>
              </span>
            </div>
            <div className="flex">
              {newPost}
              {unsub}
            </div>
          </div>

          <div className="flex" style={{ marginBottom: 24 }}>
            <Link to={base} className={tabStyles.posts}>
              All Posts
            </Link>
            <Link to={about} className={tabStyles.about}>
              About
            </Link>
            <div
              className="bb b--gray4 gray2 pv4 ph2"
              style={{ flexGrow: 1 }}></div>
          </div>

          <div style={{ height: "calc(100% - 188px)" }} className="f9 lh-solid">
            {inner}
          </div>
        </div>
      </div>
    );
  }
}

export default Notebook

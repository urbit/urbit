import React, { Component } from 'react';
import { Link, Switch, Route } from 'react-router-dom';
import { SidebarSwitcher } from './icons/icon-sidebar-switch';
import { NotebookPosts } from './notebook-posts';
import { Subscribers } from './subscribers';
import { Settings } from './settings';
import Sidebar from './sidebar';
import { cite } from '../../lib/util';

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
    if (!notebook.notes) {
      window.api.fetchNotebook(this.props.ship, this.props.book);
      return;
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

  componentDidUpdate(prevProps) {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    if (!notebook.subscribers) {
      window.api.fetchNotebook(this.props.ship, this.props.book);
    }
  }

  componentDidMount() {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    if (notebook.notes) {
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
    const { props } = this;

    // popout logic
    let hrefIndex = props.location.pathname.indexOf("/notebook/");
    let publishsubStr = props.location.pathname.substr(hrefIndex);
    let popoutHref = `/~publish/popout${publishsubStr}`;

    let hiddenOnPopout = props.popout ? "" : "dib-m dib-l dib-xl";

    let notebook = props.notebooks[props.ship][props.book];

    let tabStyles = {
      posts: "bb b--gray4 b--gray2-d gray2 pv4 ph2",
      about: "bb b--gray4 b--gray2-d gray2 pv4 ph2",
      subscribers: "bb b--gray4 b--gray2-d gray2 pv4 ph2",
      settings: "bb b--gray4 b--gray2-d pr2 gray2 pv4 ph2",
    };
    tabStyles[props.view] = "bb b--black b--white-d black white-d pv4 ph2";

    let inner = null;
    switch (props.view) {
      case "posts":
        let notesList = notebook["notes-by-date"] || [];
        let notes = notebook.notes || null;
        inner = <NotebookPosts notes={notes}
                  popout={props.popout}
                  list={notesList}
                  host={props.ship}
                  notebookName={props.book}
                  contacts={props.notebookContacts}
                  />
        break;
      case "about":
        inner = <p className="f8 lh-solid">{notebook.about}</p>
        break;
      case "subscribers":
        inner = <Subscribers
                  host={this.props.ship}
                  book={this.props.book}
                  notebook={notebook}
                  permissions={this.props.permissions}
                  groups={this.props.groups}/>
        break;
      case "settings":
        inner = <Settings
                  host={this.props.ship}
                  book={this.props.book}
                  notebook={notebook}
                  groups={this.props.groups}
                  contacts={this.props.contacts}
                  associations={this.props.associations}
                  history={this.props.history}/>
        break;
      default:
        break;
    }

    // displaying nicknames, sigil colors for contacts
    let contact = !!(props.ship.substr(1) in props.notebookContacts)
      ? props.notebookContacts[props.ship.substr(1)] : false;
    let name = props.ship;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : props.ship;
    }

    if (name === props.ship) {
      name = cite(props.ship);
    }

    let popout = (props.popout) ? "popout/" : "";
    let base = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    let about = base + '/about';
    let subs = base + '/subscribers';
    let settings = base + '/settings';
    let newUrl = base + '/new';

    let newPost = null;
    if (notebook["writers-group-path"] in props.groups){
      let writers = notebook["writers-group-path"];
      if (props.groups[writers].has(window.ship)) {
        newPost =
         <Link to={newUrl} className="NotebookButton bg-light-green green2">
           New Post
         </Link>
      }
    }

    let unsub = (window.ship === props.ship.slice(1))
      ?  null
      :  <button onClick={this.unsubscribe}
             className="NotebookButton bg-white bg-gray0-d black white-d ba b--black b--gray2-d ml3">
           Unsubscribe
         </button>

    let subsComponent = (this.props.ship.slice(1) !== window.ship)
      ? null
      : <Link to={subs} className={tabStyles.subscribers}>
          Subscribers
        </Link>;

    let settingsComponent = (this.props.ship.slice(1) !== window.ship)
      ? null
      : <Link to={settings} className={tabStyles.settings}>
          Settings
        </Link>;

    return (
      <div
        className="overflow-y-scroll"
        style={{ paddingLeft: 16, paddingRight: 16 }}
        onScroll={this.onScroll}
        ref={el => {
          this.scrollElement = el;
        }}>
        <div className="w-100 dn-m dn-l dn-xl inter pt4 pb6 f9">
          <Link to="/~publish">{"<- All Notebooks"}</Link>
        </div>
        <div className="center mw6 f9 h-100"
          style={{ paddingLeft: 16, paddingRight: 16 }}>
          <SidebarSwitcher
            popout={props.popout}
            sidebarShown={props.sidebarShown}
          />
          <Link
          className={"dn absolute right-1 top-1 " + hiddenOnPopout}
          to={popoutHref}
          target="_blank"
          >
            <img src="/~publish/popout.png"
              height={16}
              width={16}
            />
          </Link>
          <div className="h-100 pt0 pt8-m pt8-l pt8-xl no-scrollbar">
            <div
              className="flex justify-between"
              style={{ marginBottom: 32 }}>
              <div className="flex-col">
                <div className="mb1">{notebook.title}</div>
                <span>
                  <span className="gray3 mr1">by</span>
                  <span className={contact.nickname ? null : "mono"}
                  title={props.ship}>
                    {name}
                  </span>
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
              {subsComponent}
              {settingsComponent}
              <div className="bb b--gray4 b--gray2-d gray2 pv4 ph2"
                style={{ flexGrow: 1 }}></div>
            </div>

            <div style={{ height: "calc(100% - 188px)" }} className="f9 lh-solid">
              {inner}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Notebook

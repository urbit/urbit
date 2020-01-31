import React, { Component } from 'react';
import { Link, Switch, Route } from 'react-router-dom';
import { NoteList } from './note-list';
import { NotebookPosts } from './notebook-posts';
import { About } from './about';
import { Subscribers } from './subscribers';
import { Settings } from './settings';


//TODO subcomponents for posts, subscribers, settings
//
//TODO props.view switch for which component to render
//pass props.notebook, contacts to each component

//TODO ask for notebook if we don't have it
//
//TODO initialise notebook obj if no props.notebook

//TODO component bar above the rendered component
//don't render settings if it's ours
//current component is black, others gray2 (see Chat's tab bar for an example)

export class Notebook extends Component {
  constructor(props){
    super(props);
    this.state = {
      ours: (window.ship === this.props.ship.slice(1)),
      numNotes: 10,
    };

    this.onScroll = this.onScroll.bind(this);

  }

  onScroll(e) {
    let scrollTop = this.scrollElement.scrollTop;
    let clientHeight = e.target.clientHeight;
    let scrollHeight = e.target.scrollHeight;
    
    let atBottom = false;
    if (scrollHeight - scrollTop - clientHeight < 40) {
      atBottom = true;
    }
    let loadedNotes = Object.keys(this.props.notebook.notes).length;
    let allNotes = this.props.notebook["notes-by-date"].length;

    let fullyLoaded = (loadedNotes === allNotes);

    if (atBottom && !fullyLoaded) {
      window.api.fetchNotesPage(this.props.ship, this.props.notebookName, loadedNotes, 5);
    }
  }

  componentWillMount(){
    if (!this.props.notebook.notes) {
      window.api.fetchNotebook(this.props.ship, this.props.notebookName);
    }
  }

  render() {
    let tabStyles = {
      posts: "bb b--gray4 gray2 NotebookTab",
      about: "bb b--gray4 gray2 NotebookTab",
//      subscribers: "bb b--gray4 gray2 NotebookTab",
//      settings: "bb b--gray4 pr2 gray2 NotebookTab",
    }
    tabStyles[this.props.view] = "bb b--black black NotebookTab";

    let inner = null;
    switch (this.props.view) {
      case "posts":
        let notesList = this.props.notebook["notes-by-date"] || [];
        let notes = this.props.notebook.notes || null;
        inner = <NotebookPosts notes={notes}
                  list={notesList}
                  host={this.props.ship}
                  notebookName={this.props.notebookName}/>
        break;
      case "about":
        inner = <p className="f8 lh-solid">{this.props.notebook.about}</p>
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

    let base = `/~publish/notebook/${this.props.ship}/${this.props.notebookName}`;
    let about = base + '/about';
    let subs = base + '/subscribers';
    let settings = base + '/settings';
    let newUrl = base + '/new';

    return (
      <div className="center mw6 f9 h-100"
           style={{paddingLeft:16, paddingRight:16}}>
        <div className="h-100 overflow-container no-scrollbar"
             onScroll={this.onScroll}
             ref={(el) => {this.scrollElement = el}}>
          <div className="flex justify-between"
               style={{marginTop: 56, marginBottom: 32}}>
            <div className="flex-col">
              <div className="mb1">{this.props.notebook.title}</div>
              <span><span className="gray3 mr1">by</span>
                <span className="mono">
                  {this.props.ship}
                </span>
              </span>
            </div>
            <Link to={newUrl} className="NotebookButton bg-light-green green2">
              New Post
            </Link>
          </div>

          <div className="flex" style={{marginBottom:24}}>
            <Link to={base} className={tabStyles.posts}>
              All Posts
            </Link>
            <Link to={about} className={tabStyles.about}>
              About
            </Link>
            <div className="bb b--gray4 gray2 NotebookTab" style={{flexGrow:1}}>
            </div>
          </div>

          <div style={{height:"calc(100% - 188px)"}} className="f9 lh-solid">
            {inner}
          </div>
        </div>
      </div>
    )
  }
}

export default Notebook

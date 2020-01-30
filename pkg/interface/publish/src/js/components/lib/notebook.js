import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { NoteList } from './note-list';
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
    console.log("props", this.props);
  }

  render() {
    let tabStyles = {
      posts: "bb b--gray4 gray2 NotebookTab",
      about: "bb b--gray4 gray2 NotebookTab",
      subscribers: "bb b--gray4 gray2 NotebookTab",
      settings: "bb b--gray4 gray2 NotebookTab",
    }
    tabStyles[this.props.view] = "bb b--black black NotebookTab";

    return (
      <div className="center mw7 f9 h-100"
           style={{paddingLeft:16, paddingRight:16}}>
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
          <button className="NotebookButton bg-light-green green2">
            New Post
          </button>
        </div>

        <div className="flex" style={{marginBottom:24}}>
          <div className={tabStyles.posts}>
            All Posts
          </div>
          <Link to="/about" className={tabStyles.about}>
            About
          </Link>
          <div className={tabStyles.subscribers}>
            Subscribers
          </div>
          <div className={tabStyles.settings}>
            Settings
          </div>
          <div className="bb b--gray4 gray2 NotebookTab" style={{flexGrow:1}}>
          </div>
        </div>

        <div style={{height:"calc(100% - 170px)"}} className="f9 lh-solid">
          <div className="mv6">
            <div className="mb1">Title</div>
            <p className="mb1">I dreamt of urbit hardware - stars and galaxies and planets had differing...</p>
            <div className="flex">
              <div className="mono gray3 mr3">~fabled-faster</div>
              <div className="gray3 mr3">1m ago</div>
              <div className="gray3">No Comments</div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default Notebook

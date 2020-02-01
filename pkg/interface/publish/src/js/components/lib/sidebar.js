import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';
import { NotebookItem } from './notebook-item';

export class Sidebar extends Component {
  constructor(props) {
    super(props);
    this.state = {
      sort: "oldest"
    }
  }

  render() {
    const { props, state } = this;

    let activeClasses = (this.props.active === "sidebar") ? " " : "dn-s ";

    let hiddenClasses = true;

    if (this.props.popout) {
      hiddenClasses = false;
    } else {
      hiddenClasses = this.props.sidebarShown;
    };

    //TODO render notebook list from state
    // (make a new array of all notebooks from {author: {notebook}}
    // prop.notebook obj, case-switch the sorting from this.state.sort, and map it)
    //TODO allow for user sorting of notebook list 
    //
    //(reactive dropdown -> amends state -> sort by state prop)
    //
    let notebooks = <div></div>

    return (
      <div className={`bn br-m br-l br-xl b--gray4 b--gray2-d lh-copy h-100
       flex-shrink-0 mw-300-ns pt3 pt0-m pt0-l pt0-xl
        relative ` + activeClasses + ((hiddenClasses)
        ? "flex-basis-100-s flex-basis-30-ns"
        : "dn")}>
        <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">⟵ Landscape</a>
        <div className="w-100 pa4">
          <Link
          to="/~publish/new"
          className="green2 mr4 f9">
          New
          </Link>
          <Link
          to="/~publish/join"
          className="f9 gray2">
          Join
          </Link>
        </div>
        <div className="overflow-y-scroll h-100">
          <h2 className={`f8 pt1 pr4 pb3 pl3 black c-default bb b--gray4 mb2 
             dn-m dn-l dn-xl`}>
             Your Notebooks
          </h2>
          {/*TODO Dropdown attached to this.state.sort */}
          {notebooks}
        </div>
      </div>
    );
  }
}

export default Sidebar;

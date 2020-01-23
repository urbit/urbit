import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';

export class Sidebar extends Component {
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
    //TODO allow for user sorting of notebook list 
    //(reactive dropdown -> amends state -> sort by state prop)
    let notebooks = <div></div>

    return (
      <div className={`bn br-m br-l br-xl b--gray4 b--gray2-d lh-copy h-100
       flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl
        relative ` + activeClasses + ((hiddenClasses)
        ? "flex-basis-100-s flex-basis-30-ns"
        : "dn")}>
        <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">⟵ Landscape</a>
        <div className="overflow-y-scroll h-100">
          <h2 className={`f8 f9-m f9-l f9-xl 
           pt1 pt4-m pt4-l pt4-xl 
           pr4 pb3 pb2-m pb2-l pb2-xl
           pl3 pl4-m pl4-l pl4-xl 
           black-s gray2 white-d c-default
           bb bn-m bn-l bn-xl b--gray4 mb2 mb0-m mb0-l mb0-xl`}>
             Your Notebooks
             </h2>
          {notebooks}
        </div>
      </div>
    );
  }
}

export default Sidebar;

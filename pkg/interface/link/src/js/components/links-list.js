import React, { Component } from 'react'
import { LoadingScreen } from './loading';
import { LinksTabBar } from './lib/links-tabbar';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { Route, Link } from "react-router-dom";
import { LinkItem } from '/components/lib/link-item.js';
import { LinkSubmit } from '/components/lib/link-submit.js';
import { Pagination } from '/components/lib/pagination.js';

import { makeRoutePath, getContactDetails } from '../lib/util';

//TODO Avatar support once it's in
export class Links extends Component {
  constructor(props) {
    super(props);
    this.markAllAsSeen = this.markAllAsSeen.bind(this);
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate() {
    const linkPage = this.props.page;
    if ( (this.props.page != 0) &&
         (!this.props.links[linkPage] ||
          this.props.links.local[linkPage])
    ) {
      api.getPage(this.props.resourcePath, this.props.page);
    }
  }

  markAllAsSeen() {
    api.seenLink(this.props.resourcePath);
  }

  render() {
    let props = this.props;

    if (!props.resource.metadata.title) {
      return <LoadingScreen/>;
    }

    let linkPage = props.page;

    let links = !!props.links[linkPage]
    ? props.links[linkPage]
    : {};

    let currentPage = !!props.page
    ? Number(props.page)
    : 0;

    let totalPages = !!props.links
    ? Number(props.links.totalPages)
    : 1;

    let LinkList = Object.keys(links)
    .map((linkIndex) => {
      let linksObj = props.links[linkPage];
      let { title, url, time, ship } = linksObj[linkIndex];
      const seen = props.seen[url];
      let members = {};

      const commentCount = props.comments[url]
        ? props.comments[url].totalItems
        : linksObj[linkIndex].commentCount || 0;

      const {nickname, color, member} = getContactDetails(props.contacts[ship]);

      return (
        <LinkItem
        key={time}
        title={title}
        page={props.page}
        linkIndex={linkIndex}
        url={url}
        timestamp={time}
        seen={seen}
        nickname={nickname}
        ship={ship}
        color={color}
        member={member}
        comments={commentCount}
        resourcePath={props.resourcePath}
        popout={props.popout}
        />
      )
    })

    return (
      <div
      className="h-100 w-100 overflow-hidden flex flex-column">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
         <Link to="/~link">{"⟵ All Channels"}</Link>
       </div>
       <div
         className={`pl4 pt2 flex relative overflow-x-scroll
         overflow-x-auto-l overflow-x-auto-xl flex-shrink-0
         bb b--gray4 b--gray1-d bg-gray0-d`}
         style={{ height: 48 }}>
          <SidebarSwitcher
           sidebarShown={props.sidebarShown}
           popout={props.popout}/>
         <Link to={makeRoutePath(props.resourcePath, props.popout, props.page)} className="pt2">
           <h2 className={`dib f9 fw4 lh-solid v-top`}>
             {props.resource.metadata.title}
           </h2>
         </Link>
          <LinksTabBar
          {...props}
          popout={props.popout}
          page={props.page}
          resourcePath={props.resourcePath}/>
        </div>
        <div className="w-100 mt2 flex justify-center overflow-y-scroll ph4 pb4">
          <div className="w-100 mw7">
            <div className="flex">
              <LinkSubmit resourcePath={props.resourcePath}/>
            </div>
            <div className="pb4">
            <span
              className="f9 inter gray2 ba b--gray2 br2 dib pa1 pointer"
              onClick={this.markAllAsSeen}>
              mark all as seen
            </span>
            {LinkList}
            <Pagination
            {...props}
            key={props.resourcePath + props.page}
            popout={props.popout}
            resourcePath={props.resourcePath}
            currentPage={currentPage}
            totalPages={totalPages}
            />
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default Links;
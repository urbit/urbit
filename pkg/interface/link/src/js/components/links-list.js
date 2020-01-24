import React, { Component } from 'react'
import { LinksTabBar } from './lib/links-tabbar';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { Route, Link } from "react-router-dom";
import { LinkItem } from '/components/lib/link-item.js';
import { LinkSubmit } from '/components/lib/link-submit.js';
import { Pagination } from '/components/lib/pagination.js';

//TODO look at uxToHex wonky functionality
import { uxToHex } from '../lib/util';

//TODO Avatar support once it's in
export class Links extends Component {

  componentDidMount() {
    let linkPage = "page" + this.props.page;
    if ((this.props.page !== 0) && (!this.props.links[linkPage])) {
      api.getPage(this.props.path, this.props.page);
    }
  }

  componentDidUpdate() {
    let linkPage = "page" + this.props.page;
    if ((this.props.page !== 0) && (!this.props.links[linkPage])) {
      api.getPage(this.props.path, this.props.page);
    }
  }

  render() {
    let props = this.props;
    let popout = (props.popout) ? "/popout" : "";
    let channel = props.path.substr(1);
    let linkPage = "page" + props.page;

    let links = !!props.links[linkPage]
    ? props.links[linkPage]
    : {};

    let currentPage = !!props.page
    ? Number(props.page)
    : 0;

    let totalPages = !!props.links
    ? Number(props.links["total-pages"])
    : 1;

    let LinkList = Object.keys(links)
    .map((link) => {
      let linksObj = props.links[linkPage];
      let { title, url, timestamp, ship, commentCount } = linksObj[link];
      let members = {};

      if (!props.members[ship]) {
        members[ship] = {'nickname': '', 'avatar': 'TODO', 'color': '0x0'};
      } else {
        members = props.members;
      }

      let color = uxToHex('0x0');
      let nickname = "";

      // restore this to props.members
      if (members[ship].nickname) {
        nickname = members[ship].nickname;
      }

      if (members[ship].color !== "") {
        color = uxToHex(members[ship].color);
      }

      return (
        <LinkItem
        key={timestamp}
        title={title}
        page={props.page}
        index={link}
        url={url}
        timestamp={timestamp}
        nickname={nickname}
        ship={ship}
        color={color}
        comments={commentCount}
        channel={channel}
        popout={popout}
        />
      )
    })

    return (
      <div
      className="h-100 w-100 overflow-hidden flex flex-column">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
         <Link to="/~link/">{"⟵ All Channels"}</Link>
       </div>
       <div
         className={`pl3 pt2 flex relative overflow-x-scroll 
         overflow-x-auto-l overflow-x-auto-xl flex-shrink-0
         bb bn-m bn-l bn-xl b--gray4`}
         style={{ height: 48 }}>
          <SidebarSwitcher
           sidebarShown={props.sidebarShown}
           popout={props.popout}/>
         <Link to={`/~link` + popout + props.path} className="pt2">
           <h2
             className={`dib f8 fw4 v-top ` + 
             (props.path.includes("/~/")
             ? ""
             : "mono")}>
              {(props.path.includes("/~/"))
              ? "Private"
              : channel}
           </h2>
         </Link>
          <LinksTabBar
          {...props}
          popout={popout}
          path={props.path}/>
        </div>
        <div className="w-100 mt2 flex justify-center overflow-y-scroll ph4 pb4">
          <div className="w-100 mw7">
            <div className="flex">
              <LinkSubmit path={props.path}/>
            </div>
            <div className="pb4">
            {LinkList}
            <Pagination
            {...props}
            key={props.path + props.page}
            popout={popout}
            path={props.path}
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
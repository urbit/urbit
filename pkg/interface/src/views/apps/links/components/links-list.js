import React, { Component } from 'react';

import { LoadingScreen } from './loading';
import { MessageScreen } from './lib/message-screen';
import { LinksTabBar } from './lib/links-tabbar';
import { SidebarSwitcher } from '~/views/components/SidebarSwitch';
import { Link } from 'react-router-dom';
import { LinkItem } from './lib/link-item';
import { LinkSubmit } from './lib/link-submit';
import { Pagination } from './lib/pagination';

import { makeRoutePath, getContactDetails } from '~/logic/lib/util';

export class Links extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate(prevProps) {
    const linkPage = this.props.page;
    // if we just navigated to this particular page,
    // and don't have links for it yet,
    // or the links we have might not be complete,
    // request the links for that page.
    if ( ((!prevProps || // first load?
          linkPage !== prevProps.page || // already waiting on response?
          this.props.resourcePath !== prevProps.resourcePath // new page?
         ) ||
         (prevProps.api !== this.props.api)) // api prop instantiated?
         &&
         !this.props.links[linkPage] || // don't have info?
         this.props.links.local[linkPage] // waiting on post confirmation?
    ) {
      this.props.api?.links.getPage(this.props.resourcePath, this.props.page);
    }
  }

  render() {
    const props = this.props;

    if (!props.resource.metadata.title) {
      return <LoadingScreen />;
    }

    const linkPage = props.page;

    const links = props.links[linkPage]
    ? props.links[linkPage]
    : {};

    const currentPage = props.page
    ? Number(props.page)
    : 0;

    const totalPages = props.links
    ? Number(props.links.totalPages)
    : 1;

    let LinkList = (<LoadingScreen />);
    if (props.links && props.links.totalItems === 0) {
      LinkList = (
        <MessageScreen text="Start by posting a link to this collection." />
      );
    } else if (Object.keys(links).length > 0) {
      LinkList = Object.keys(links)
      .map((linkIndex) => {
        const linksObj = props.links[linkPage];
        const { title, url, time, ship } = linksObj[linkIndex];
        const seen = props.seen[url];

        const commentCount = props.comments[url]
          ? props.comments[url].totalItems
          : linksObj[linkIndex].commentCount || 0;

        const { nickname, color, member, avatar } = getContactDetails(props.contacts[ship]);

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
          avatar={avatar}
          member={member}
          comments={commentCount}
          resourcePath={props.resourcePath}
          popout={props.popout}
          api={props.api}
          />
        );
      });
    }

    return (
      <div
      className="h-100 w-100 overflow-hidden flex flex-column"
      >
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: '1rem' }}
        >
         <Link to="/~link">{'⟵ All Channels'}</Link>
       </div>
       <div
         className={`pl4 pt2 flex relative overflow-x-scroll
         overflow-x-auto-l overflow-x-auto-xl flex-shrink-0
         bb b--gray4 b--gray1-d bg-gray0-d`}
         style={{ height: 48 }}
       >
          <SidebarSwitcher
           sidebarShown={props.sidebarShown}
           popout={props.popout}
           api={this.props.api}
          />
         <Link to={makeRoutePath(props.resourcePath, props.popout, props.page)} className="pt2">
           <h2 className={'dib f9 fw4 lh-solid v-top'}>
             {props.resource.metadata.title}
           </h2>
         </Link>
          <LinksTabBar
          {...props}
          popout={props.popout}
          page={props.page}
          resourcePath={props.resourcePath}
          />
        </div>
        <div className="w-100 mt6 flex justify-center overflow-y-scroll ph4 pb4">
          <div className="w-100 mw7">
            <div className="flex">
              <LinkSubmit resourcePath={props.resourcePath} api={this.props.api} s3={props.s3} />
            </div>
            <div className="pb4">
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
    );
  }
}

export default Links;

import React, { Component } from 'react';
import classnames from 'classnames';
import { Scrollbars } from 'react-custom-scrollbars';
import moment from 'moment';

import { getMessageContent } from '/lib/util';
import { SidebarItem } from '/components/sidebar-item';

export class Sidebar extends Component {
  render() {
    const { props } = this;
    let station = props.match.params.ship + '/' + props.match.params.station;

    let sidebarItems = props.circles.map((cir) => {
      let msg = props.messagePreviews[cir];
      let parsed = getMessageContent(msg);
      let wen = moment.unix(msg.wen / 1000).from(moment.utc());

      return (
        <CollectionItem
          title={cir}
          description={parsed.content}
          datetime={wen}
          selected={station === cir}
          history={props.history}
        />
      );
    });

    return (

      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className="pl3 pr3 pt2 pb2 cf">
          <h2 className="dib lh-title sans-serif w-50 f2">Publish</h2>
          <a className="dib tr lh-title sans-serif w-50 f4 underline">+ New</a>
        </div>
        <div className='mt2 pl3 pr3 mb2 w-100'>
          <div>My Collections</div>
        </div>
        <div style={{ flexGrow: 1 }}>
          <Scrollbars
            ref={this.scrollbarRef}
            renderTrackHorizontal={props => <div style={{display: "none"}}/>}
            onScrollStop={this.onScrollStop}
            renderView={props => <div {...props} />}
            style={{ height: '100%' }}
            autoHide>
            {sidebarItems}
          </Scrollbars>
        </div>
        <div className='mt2 pl3 pr3 mb2 w-100'>
          <div>My Subscriptions</div>
        </div>
      </div>
    )
  }
}


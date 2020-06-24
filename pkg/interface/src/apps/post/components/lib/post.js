import React, { Component } from 'react';
import { OverlaySigil } from './overlay-sigil';
import PostContent from './post-content';
import { uxToHex, cite, writeText } from '../../../../lib/util';
import moment from 'moment';


export class Post extends Component {
  render() {
    const { props, state } = this;
    const datestamp = '~' + moment.unix(props.msg['time-sent'] / 1000).format('YYYY.M.D');

    const paddingTop = { 'paddingTop': '6px' };

    const timestamp = moment.unix(props.msg['time-sent'] / 1000).format('hh:mm a');

    let name = `~${props.msg.author}`;
    let color = '#000000';
    let sigilClass = 'mix-blend-diff';

    const bodyFont = !!props.isParent ? "f6" : "f8";
    const smallFont = !!props.isParent ? "f7" : "f9";

    return (
      <div
        ref={this.containerRef}
        className={
          `w-100 ${bodyFont} pl3 pt4 pr3 cf flex lh-copy bt b--white pointer`
        }
        style={{
          minHeight: 'min-content'
        }}
        onClick={() => {
          props.history.push(`/~post/room/` +
            `${props.resource.ship}/${props.resource.name}${props.index}`);
        }} 
      >
       <OverlaySigil
         ship={props.msg.author}
         color={'#000'}
         sigilClass={sigilClass}
         group={props.group}
         className="fl pr3 v-top bg-white bg-gray0-d"
       />
        <div
          className="fr clamp-message white-d"
          style={{ flexGrow: 1, marginTop: -8 }}
        >
          <div className="hide-child" style={paddingTop}>
            <p className={`v-mid ${smallFont} gray2 dib mr3 c-default`}>
              <span>{`~${props.msg.author}`}</span>
            </p>
            <p className={`v-mid mono ${smallFont} gray2 dib`}>{timestamp}</p>
            <p className={`v-mid mono ${smallFont} ml2 gray2 dib child dn-s`}>
              {datestamp}
            </p>
          </div>
          <PostContent
            contents={props.msg.contents}
            isParent={props.isParent} />
        </div>
      </div>
    );
  }
}

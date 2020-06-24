import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';

import { Link } from 'react-router-dom';

import { PostList } from './lib/post-list';
import { Post } from './lib/post';
import { PostInput } from './lib/post-input';
import { deSig } from '../../../lib/util';

export class NodeTreeScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {};

    moment.updateLocale('en', {
      calendar: {
        sameDay: '[Today]',
        nextDay: '[Tomorrow]',
        nextWeek: 'dddd',
        lastDay: '[Yesterday]',
        lastWeek: '[Last] dddd',
        sameElse: 'DD/MM/YYYY'
      }
    });
  }

  parentPost() {
    const { props } = this;
    const node = props.node;

    let prevIndex = node.post.index.split('/');
    prevIndex.pop();
    prevIndex = prevIndex.join('/');
    
    return (
      <div>
        <span className="dib f9 v-mid gray2 ml1 mr1 c-default inter">
          <Link className="dib f9 v-mid inter ml2 no-underline white-d"
             to={
               "/~post/room/" +
               `${props.resource.ship}/${props.resource.name}` +
               `${prevIndex}`
             }>
            ⟵
          </Link>
        </span>
        <Post
          isParent={true}
          key={node.post.index}
          msg={node.post}
        />
      </div>
    );
  }

  replyModal() {
    const { props, state } = this;

    return (
      <div>
        <PostInput
          api={props.api}
          resource={props.resource}
          owner={deSig(props.match.params.ship)}
          placeholder="Message..."
          parentIndex={props.parentIndex}
        />
      </div>
    );
  }

  render() {
    const { props } = this;

    return (
      <div key={props.resource.name}
           className="h-100 w-100 overflow-hidden flex flex-column relative">
        <div className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
             style={{ height: '1rem' }}>
          <Link to="/~post/">{'⟵ All Graphs'}</Link>
        </div>
        <div
          className={
            'pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d' +
            'flex relative flex-shrink-0' +
            'overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl' 
          }
          style={{ height: 48 }}>
          <Link to={`/~post/room/${props.resource.ship}/${props.resource.name}`}
                className="pt2 white-d">
            <h2 className={'dib f9 fw4 lh-solid v-top '}
                style={{ width: 'max-content' }}>
              {props.resource.name}
            </h2>
          </Link>
        </div>
        {this.parentPost()}
        {this.replyModal()}
        <PostList
          api={props.api}
          graph={props.node.children}
          history={props.history}
          resource={props.resource}
        />
      </div>
    );
  }
}

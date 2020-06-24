import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { Post } from './lib/post';
import { PostInput } from './lib/post-input';
import { PostList } from './lib/post-list';
import { deSig } from '../../../lib/util';


export class PostScreen extends Component {
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
        <PostInput
          api={props.api}
          resource={props.resource}
          owner={deSig(props.match.params.ship)}
          placeholder="Post..."
        />
        <PostList
          api={props.api}
          graph={props.graph}
          history={props.history}
          resource={props.resource}
        />
      </div>
    );
  }
}

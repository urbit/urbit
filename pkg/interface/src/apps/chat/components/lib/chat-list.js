import React, { Component, Fragment, forwardRef } from "react";
import InfiniteLoader from 'react-window-infinite-loader';
import { DynamicSizeList } from 'react-window-dynamic';

import { ChatMessage } from './chat-message';


export default class ChatList extends Component {

  isItemLoaded(index) {
    return true;
  }

  loadMoreItems(startIndex, stopIndex) {
    console.log('fetch backlog');
    return new Promise();
  }

  render() {
    const { props } = this;
/*
      <InfiniteLoader
        minimumBatchSize={300}
        threshold={20}
        itemCount={props.messages.length}
        loadMoreItems={this.loadMoreItems.bind(this)}
        isItemLoaded={this.isItemLoaded.bind(this)}>
        {({ onItemsRendered, ref }) => (
        )}
      </InfiniteLoader>
*/

    return (
      <DynamicSizeList
        height={props.height}
        width={props.width}
        itemCount={props.messages.length}>
        { forwardRef((pro, ref) => (
            <ChatMessage
              ref={ref}
              previousMsg={props.messages[pro.index - 1]}
              nextMsg={props.messages[pro.index + 1]}
              msg={props.messages[pro.index]}
              index={pro.index}
              contacts={props.contacts}
            />
          ))
        }
      </DynamicSizeList>
    );
  }
}

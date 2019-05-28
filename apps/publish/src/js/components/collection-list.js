import React, { Component } from 'react';
import classnames from 'classnames';


export class CollectionList extends Component {
  render() {
    console.log("collection-list.props", this.props);
  
    let listItems = this.props.list.map((coll) => {
      return (
        <p className="w-100">
          {coll.data.info.title}
        </p>
      );

    });

    console.log(listItems);

    return (
      <div className="w-100">
        {listItems}
      </div>
    );
  }
}


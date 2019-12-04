import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom'; 

export class GroupsItem extends Component {
    render() {
        const { props } = this;
        console.log(props.group)

        return (
            <Link
            to={"/" + props.link}>
                <div className="w-100 v-mid f9 pl4">
                <p>{Object.keys(props.group)}</p>
                </div>
            </Link>
        )
    }
}

export default GroupsItem

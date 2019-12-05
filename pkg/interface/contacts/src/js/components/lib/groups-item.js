import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom'; 

export class GroupsItem extends Component {
    render() {
        const { props } = this;

        return (
            <Link
            to={"/~contacts" + props.link}>
                <div className="w-100 v-mid f9 pl4">
                <p>{props.name}</p>
                </div>
            </Link>
        )
    }
}

export default GroupsItem

import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom'; 

export class GroupsItem extends Component {
    render() {
        const { props } = this;

        let selectedClass = (props.selected)
        ? "bg-gray4"
        : null;

        let memberCount = Object.keys(props.contacts).length;

        return (
            <Link
            to={"/~contacts" + props.link}>
                <div className={"w-100 v-mid f9 pl4 " + selectedClass}>
                <p className="f9 pt1">{props.name}</p>
                <p className="f9 pb1 gray2">{memberCount + " Member" + ((memberCount === 1) ? "" : "s")}</p>
                </div>
            </Link>
        )
    }
}

export default GroupsItem

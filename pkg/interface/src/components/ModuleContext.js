import * as React from 'react'
import { Row , Col } from '@tlon/indigo-react'
import { useLocation } from 'react-router-dom';

// ModuleContext is the outermost wrapper of each module. It handles the padding between the edge of the window and the module rectangle, its responsive styling and popout styling.

const ModuleContext = ({ children}) => {
    const {pathname} = useLocation()

    const popout = pathname.includes('/popout/')

    return (
        <Row height={popout ? '100vh' : 'calc(100% - 45px)'} p={popout ? '0' : '3'} pt='0'>
            <Col border='1px solid' borderColor={popout ? 'white' : 'washedGray'} pt='0'>
                {
                    children
                }
        </Col>
    </Row>
    )
}

export default ModuleContext
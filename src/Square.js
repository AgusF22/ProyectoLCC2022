import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {
    render() {
        return (
            <div className={"boardCell"} style={{ backgroundColor: colorToCss(this.props.value) }} />
        );
    }
}

export default Square;
import Image from 'next/image';
import styles from './About.module.css';

export default function About() {
    return (
        <main className={styles.container}>
            <div className={styles.postcard}>
                <div className={styles.imageSection}>
                    <Image
                        src="/placeholder/erandi-portrait.jpg"
                        alt="Erandi"
                        fill
                        className={styles.image}
                        priority
                    />
                </div>
                <div className={styles.textSection}>
                    <p className={styles.text}>
                        We believe in spaces that breathe. Our approach is rooted in the interplay of light, material, and silence.
                        <br /><br />
                        Every project is a dialogue between the architecture and the inhabitants, resulting in environments that feel both curated and deeply personal.
                        <br /><br />
                        Not just to look at, but to live in.
                    </p>
                </div>
            </div>
        </main>
    );
}
